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
import Spesa.Section

/** `spesa image...` prints the sorted lists; with no arguments it runs the Telegram bot (long polling). */
object Main extends IOApp:
  // files edited by the bot live in the state directory and take precedence over the built-in ones
  val state        = Path(sys.env.getOrElse("STATE_DIRECTORY", "."))
  val learnedFile  = state / "learned.txt"
  val cristinaFile = state / "cristina.txt"
  val repartiFile  = state / "reparti.txt"

  def read(file: Path): IO[Option[String]] = Files[IO].exists(file).ifM(Files[IO].readUtf8(file).compile.string.map(Some(_)), IO.none)

  def write(file: Path, text: String): IO[Unit] = Stream.emit(text + "\n").through(Files[IO].writeUtf8(file)).compile.drain

  val commands = List(
    "reparti"    -> "mostra Cristina e i reparti con il loro contenuto",
    "metti"      -> "reparto voce: mette la voce nel reparto (o in cristina), creandolo se serve",
    "togli"      -> "voce: la toglie da Cristina e dai reparti",
    "sposta"     -> "reparto N: sposta il reparto in posizione N",
    "dizionario" -> "mostra le parole imparate",
    "aggiungi"   -> "parola...: le aggiunge al dizionario",
    "rimuovi"    -> "parola...: le rimuove dal dizionario")

  def run(args: List[String]): IO[ExitCode] =
    val learned  = read(learnedFile).map(_.fold(Set.empty)(Spesa.entries(_).toSet))
    val sections = (read(cristinaFile), read(repartiFile)).flatMapN { (c, r) =>
      IO.fromEither(Spesa.parseSections(c.getOrElse(Spesa.resource("/cristina.txt")), r.getOrElse(Spesa.resource("/reparti.txt")))
        .leftMap(IllegalArgumentException(_)))
    }
    (learned, sections).flatMapN { (learned, sections) =>
      if args.nonEmpty then args.traverse_(f => ocr(Files[IO].readAll(Path(f))).flatMap { t =>
        val r = Spesa.fromOcr(t, Spesa.builtin ++ learned)
        IO.println(s"=== $f\n${show(Spesa.sort(r.items, sections))}" + r.ignored.map("\n  [ignored] " + _).mkString)
      })
      else EmberClientBuilder.default[IO].withTimeout(60.seconds).build.use { c =>
        (Ref.of[IO, Set[String]](learned), Ref.of[IO, List[Section]](sections)).flatMapN(bot(c, _, _))
      }
    }.as(ExitCode.Success)

  def show(groups: List[(String, List[String])]): String =
    groups.map((name, items) => (name.toUpperCase :: items).mkString("\n")).mkString("\n\n")

  /** `learned` holds the words added at runtime, on top of the built-in vocabulary. */
  def bot(client: Client[IO], learned: Ref[IO, Set[String]], sections: Ref[IO, List[Section]]): IO[Unit] =
    val token = sys.env("TELEGRAM_TOKEN")
    // only these Telegram user ids are answered; anyone can find and message a bot
    val users = sys.env("SPESA_USERS").split(",").map(_.trim.toLong).toSet

    def call(method: String, params: (String, String)*): IO[ACursor] =
      client.expect[Json](Request[IO](Method.POST, Uri.unsafeFromString(s"https://api.telegram.org/bot$token/$method"))
        .withEntity(UrlForm(params*))).map(_.hcursor.downField("result"))

    // request URLs contain the token, so it never goes into logs or chats
    def redact(e: Throwable) = e.toString.replace(token, "<token>")

    // Telegram caps a message at 4096 characters
    def send(chat: Long, text: String) = text.grouped(4000).toList.traverse_(t => call("sendMessage", "chat_id" -> chat.toString, "text" -> t))

    def download(fileId: String): Stream[IO, Byte] = Stream.eval(call("getFile", "file_id" -> fileId).flatMap(r => IO.fromEither(r.get[String]("file_path")))).flatMap { path =>
      client.stream(Request[IO](uri = Uri.unsafeFromString(s"https://api.telegram.org/file/bot$token/$path"))).flatMap(_.body)
    }

    def vocab = learned.get.map(Spesa.builtin ++ _)

    def updateWords(f: Set[String] => Set[String]): IO[Unit] =
      learned.updateAndGet(f).flatMap(ws => write(learnedFile, ws.toList.sorted.mkString("\n")))

    def showSections(ss: List[Section]) = s"[${Spesa.Cristina}]\n${Spesa.showCristina(ss)}\n\n${Spesa.showReparti(ss)}"

    // supermarket sections left empty disappear; Cristina always stays first
    def updateSections(f: List[Section] => List[Section]): IO[String] =
      sections.updateAndGet(ss => f(ss) match { case c :: rest => c :: rest.filter(_.items.nonEmpty); case Nil => Nil })
        .flatTap(ss => write(cristinaFile, Spesa.showCristina(ss)) *> write(repartiFile, Spesa.showReparti(ss)))
        .map(showSections)

    def without(ss: List[Section], item: String) = ss.map(s => s.copy(items = s.items.filterNot(_ == item)))

    def command(name: String, rest: String): IO[String] =
      val words = Spesa.words(rest).distinct
      val (target, item) = rest.trim.span(!_.isWhitespace).bimap(identity, Spesa.normalize)
      name match
        case "/dizionario" => learned.get.map(ws => if ws.isEmpty then "Nessuna parola imparata" else ws.toList.sorted.mkString("\n"))
        case "/aggiungi" if words.nonEmpty => updateWords(_ ++ words).as(s"Aggiunte: ${words.mkString(", ")}")
        case "/rimuovi" if words.nonEmpty =>
          learned.get.flatMap { ws =>
            val (gone, kept) = words.partition(ws)
            updateWords(_ -- gone).as((s"Rimosse: ${gone.mkString(", ")}" +: kept.map(w => s"Non imparata: $w")).mkString("\n"))
          }
        case "/reparti" => sections.get.map(showSections)
        case "/metti" if item.nonEmpty => // "/metti frigo pasta sfoglia": moves the item there, creating the section if new
          updateSections { ss =>
            val cleaned = without(ss, item)
            if cleaned.exists(_.name.equalsIgnoreCase(target)) then
              cleaned.map(s => if s.name.equalsIgnoreCase(target) then s.copy(items = (s.items :+ item).sorted) else s)
            else cleaned :+ Section(target.capitalize, List(item))
          }
        case "/togli" if rest.trim.nonEmpty => updateSections(without(_, Spesa.normalize(rest)))
        case "/sposta" => // "/sposta pesce 1": position among the supermarket sections
          rest.trim.split("\\s+") match
            case Array(section, pos) if pos.toIntOption.isDefined =>
              updateSections {
                case c :: ss =>
                  val (moving, others) = ss.partition(_.name.equalsIgnoreCase(section))
                  c :: others.patch(pos.toInt - 1, moving, 0)
                case Nil => Nil
              }
            case _ => IO.pure("Uso: /sposta reparto posizione")
        case _ => IO.pure(commands.map((c, d) => s"/$c $d").mkString("\n"))

    def handle(msg: ACursor, chat: Long): IO[Unit] =
      // screenshots arrive as a photo (largest size last) or, sent uncompressed, as a document
      val fileId = msg.downField("photo").as[List[Json]].toOption.flatMap(_.lastOption).flatMap(_.hcursor.get[String]("file_id").toOption)
        .orElse(msg.downField("document").get[String]("file_id").toOption)
      (fileId, msg.get[String]("text").toOption) match
        case (Some(id), _) =>
          for
            text <- ocr(download(id))
            r    <- vocab.map(Spesa.fromOcr(text, _))
            _    <- send(chat, if r.items.isEmpty then "Nessun elemento trovato" else r.items.mkString("\n"))
            _    <- send(chat, "Righe ignorate:\n" + r.ignored.mkString("\n")).whenA(r.ignored.nonEmpty)
          yield ()
        case (_, Some(text)) if text.startsWith("/") =>
          val name = text.takeWhile(!_.isWhitespace).takeWhile(_ != '@') // "/reparti@SpesaBot" in groups
          command(name.toLowerCase, text.dropWhile(!_.isWhitespace)).flatMap(send(chat, _))
        case (_, Some(text)) => // the corrected list: learn its new words, reply sorted
          for
            known  <- vocab
            fresh   = Spesa.words(text).filterNot(known).distinct
            _      <- updateWords(_ ++ fresh).whenA(fresh.nonEmpty)
            items  <- vocab.map(Spesa.items(text, _))
            groups <- sections.get.map(Spesa.sort(items, _))
            (cristina, market) = groups.partition(_._1 == Spesa.Cristina)
            // Cristina's list is plain so it can be forwarded as is
            _      <- cristina.traverse_((_, is) => send(chat, is.mkString("\n")))
            _      <- send(chat, show(market)).whenA(market.nonEmpty)
          yield ()
        case _ => IO.unit

    def poll(offset: Long): IO[Unit] =
      call("getUpdates", "offset" -> offset.toString, "timeout" -> "50", "allowed_updates" -> """["message"]""")
        .map(_.as[List[Json]].getOrElse(Nil))
        .flatMap(_.traverse { u =>
          val msg  = u.hcursor.downField("message")
          val user = msg.downField("from").get[Long]("id").toOption
          val chat = msg.downField("chat").get[Long]("id").toOption.filter(_ => user.exists(users))
          chat.traverse_(c => handle(msg, c).handleErrorWith(e => IO.println(redact(e)) *> send(c, s"Errore: ${redact(e)}").attempt.void))
            .as(u.hcursor.get[Long]("update_id").getOrElse(offset - 1) + 1)
        })
        .map(_.maxOption.getOrElse(offset))
        .handleErrorWith(e => IO.println(s"poll failed: ${redact(e)}") *> IO.sleep(5.seconds).as(offset))
        .flatMap(poll)

    // shows the commands with their descriptions in Telegram's "/" menu
    val menu = Json.arr(commands.map((c, d) => Json.obj("command" -> Json.fromString(c), "description" -> Json.fromString(d)))*)
    call("setMyCommands", "commands" -> menu.noSpaces).void.handleErrorWith(e => IO.println(s"setMyCommands failed: ${redact(e)}")) *> poll(0)
