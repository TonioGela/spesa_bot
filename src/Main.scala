import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.syntax.all.*
import fs2.Stream
import fs2.io.file.{Files, Path}
import io.circe.{ACursor, Json}
import org.http4s.{Method, Request, Uri, UrlForm}
import org.http4s.circe.*
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import scala.concurrent.duration.*
import Spesa.Config

/** `spesa image...` prints the sorted lists; with no arguments it runs the Telegram bot (long polling). */
object Main extends IOApp:
  // once edited by the bot, the config lives in the state directory and takes precedence over the built-in one
  val configFile = Path(sys.env.getOrElse("STATE_DIRECTORY", ".")) / "config.txt"

  val commands = List("config" -> "mostra la configurazione; seguito dal testo nuovo, la sostituisce")

  // ponytail: whole file in one message, switch to sending it as a document if it outgrows 4096 characters
  def configText: IO[String] = Files[IO].exists(configFile).ifM(Files[IO].readUtf8(configFile).compile.string, IO(Spesa.builtin))

  def run(args: List[String]): IO[ExitCode] =
    configText.flatMap(t => IO.fromEither(Spesa.parseConfig(t).leftMap(IllegalArgumentException(_)))).flatMap { config =>
      if args.nonEmpty then args.traverse_(f => ocr(Files[IO].readAll(Path(f))).flatMap { t =>
        val r = Spesa.fromOcr(t, config)
        IO.println(s"=== $f\n${show(Spesa.sort(r.items, config))}" + r.ignored.map("\n  [ignored] " + _).mkString)
      })
      else
        // the token is read from a file when TELEGRAM_TOKEN_FILE is set, so it stays out of the environment and the Nix store
        val token = sys.env.get("TELEGRAM_TOKEN_FILE").fold(IO(sys.env("TELEGRAM_TOKEN")))(f => Files[IO].readUtf8(Path(f)).compile.string.map(_.trim))
        (token, Ref.of[IO, Config](config)).flatMapN((t, ref) => EmberClientBuilder.default[IO].withTimeout(60.seconds).build.use(bot(_, ref, t)))
    }.as(ExitCode.Success)

  def show(groups: List[(String, List[String])]): String =
    groups.map((name, items) => (name.toUpperCase :: items).mkString("\n")).mkString("\n\n")

  def bot(client: Client[IO], config: Ref[IO, Config], token: String): IO[Unit] =
    // only these Telegram user ids are answered; anyone can find and message a bot
    val users = sys.env("SPESA_USERS").split(",").map(_.trim.toLong).toSet

    def call(method: String, params: (String, String)*): IO[ACursor] =
      client.expect[Json](Request[IO](Method.POST, Uri.unsafeFromString(s"https://api.telegram.org/bot$token/$method"))
        .withEntity(UrlForm(params*))).map(_.hcursor.downField("result"))

    // stderr is not buffered, so logs show up even if the process is killed
    def log(line: String) = cats.effect.std.Console[IO].errorln(line)

    // request URLs contain the token, so it never goes into logs or chats
    def redact(e: Throwable) = e.toString.replace(token, "<token>")

    // Telegram caps a message at 4096 characters
    def send(chat: Long, text: String) = text.grouped(4000).toList.traverse_(t => call("sendMessage", "chat_id" -> chat.toString, "text" -> t))

    def download(fileId: String): Stream[IO, Byte] = Stream.eval(call("getFile", "file_id" -> fileId).flatMap(r => IO.fromEither(r.get[String]("file_path")))).flatMap { path =>
      client.stream(Request[IO](uri = Uri.unsafeFromString(s"https://api.telegram.org/file/bot$token/$path"))).flatMap(_.body)
    }

    def command(name: String, rest: String): IO[String] = name match
      case "/config" if rest.trim.isEmpty => configText
      case "/config" => // the edited file, saved as sent so its comments survive
        Spesa.parseConfig(rest).fold(
          err => IO.pure(s"Configurazione non valida, tengo la vecchia: $err"),
          c => Stream.emit(rest.trim + "\n").through(Files[IO].writeUtf8(configFile)).compile.drain *> config.set(c)
            .as(s"Configurazione aggiornata: ${c.sections.length} reparti, ${c.fixes.size} correzioni"))
      case _ => IO.pure(commands.map((c, d) => s"/$c $d").mkString("\n"))

    def handle(msg: ACursor, chat: Long): IO[Unit] =
      // screenshots arrive as a photo (largest size last) or, sent uncompressed, as a document
      val fileId = msg.downField("photo").as[List[Json]].toOption.flatMap(_.lastOption).flatMap(_.hcursor.get[String]("file_id").toOption)
        .orElse(msg.downField("document").get[String]("file_id").toOption)
      (fileId, msg.get[String]("text").toOption) match
        case (Some(id), _) =>
          for
            text <- ocr(download(id))
            r    <- config.get.map(Spesa.fromOcr(text, _))
            _    <- send(chat, if r.items.isEmpty then "Nessun elemento trovato" else r.items.mkString("\n"))
            _    <- send(chat, "Righe ignorate:\n" + r.ignored.mkString("\n")).whenA(r.ignored.nonEmpty)
          yield ()
        case (_, Some(text)) if text.startsWith("/") =>
          val name = text.takeWhile(!_.isWhitespace).takeWhile(_ != '@') // "/reparti@SpesaBot" in groups
          command(name.toLowerCase, text.dropWhile(!_.isWhitespace)).flatMap(send(chat, _))
        case (_, Some(text)) => // the corrected list: reply sorted
          config.get.flatMap { c =>
            val groups = Spesa.sort(Spesa.list(text, c), c)
            val (cristina, rest)    = groups.partition(_._1 == Spesa.Cristina)
            val (missing, market)   = rest.partition(_._1 == Spesa.Missing)
            // Cristina's list is plain so it can be forwarded as is
            cristina.traverse_((_, is) => send(chat, is.mkString("\n"))) *>
              send(chat, show(market)).whenA(market.nonEmpty) *>
              missing.traverse_((_, is) => send(chat, s"${Spesa.Missing} (aggiungile a un reparto con /config):\n${is.mkString("\n")}"))
          }
        case _ => IO.unit

    def poll(offset: Long): IO[Unit] =
      call("getUpdates", "offset" -> offset.toString, "timeout" -> "50", "allowed_updates" -> """["message"]""")
        .map(_.as[List[Json]].getOrElse(Nil))
        .flatMap(_.traverse { u =>
          val msg  = u.hcursor.downField("message")
          val user = msg.downField("from").get[Long]("id").toOption
          val chat = msg.downField("chat").get[Long]("id").toOption.filter(_ => user.exists(users))
          log(s"ignored message from user ${user.getOrElse("?")}").whenA(chat.isEmpty) *>
          chat.traverse_(c => handle(msg, c).handleErrorWith(e => log(redact(e)) *> send(c, s"Errore: ${redact(e)}").attempt.void))
            .as(u.hcursor.get[Long]("update_id").getOrElse(offset - 1) + 1)
        })
        .map(_.maxOption.getOrElse(offset))
        .handleErrorWith(e => log(s"poll failed: ${redact(e)}") *> IO.sleep(5.seconds).as(offset))
        .flatMap(poll)

    // shows the commands with their descriptions in Telegram's "/" menu
    val menu = Json.arr(commands.map((c, d) => Json.obj("command" -> Json.fromString(c), "description" -> Json.fromString(d)))*)
    call("setMyCommands", "commands" -> menu.noSpaces).void.handleErrorWith(e => log(s"setMyCommands failed: ${redact(e)}")) *> poll(0)
