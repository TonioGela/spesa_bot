import scala.io.Source

/** Grocery-list cleanup: drops non-list lines, rejoins split words, fixes spelling against a vocabulary. */
object Spesa:
  case class Result(items: List[String], ignored: List[String])

  /** Non-empty lines of a text file, without `#` comments. */
  def entries(text: String): List[String] = text.linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toList

  def resource(name: String): String = Source.fromInputStream(getClass.getResourceAsStream(name)).mkString

  lazy val builtin: Set[String] = entries(resource("/vocabulary.txt")).toSet

  private val Word = raw"\p{L}+(?:'\p{L}+)?".r

  def words(text: String): List[String] = Word.findAllIn(text.toLowerCase).toList

  /** Text of an OCR'd screenshot -> items. Lines without any known word (status bar, dates) are set aside. */
  def fromOcr(text: String, vocab: Set[String]): Result =
    val lines = text.replace('|', 'l').linesIterator.map(_.trim).filter(_.nonEmpty)
    // rejoin words split across lines: "formag-" "gio", "li" "mone"; the latest line is the head
    val merged = lines.foldLeft(List.empty[String]) {
      case (prev :: acc, next) if prev.endsWith("-")            => (prev.init + next) :: acc
      case (prev :: acc, next) if splitWord(prev, next, vocab) => (prev + next) :: acc
      case (acc, next)                                          => next :: acc
    }.reverse
    val (kept, ignored) = merged.partition(l => words(l).exists(w => w.length >= 3 && fix(w, vocab).isDefined))
    Result(items(kept.mkString(" "), vocab), ignored)

  /** The last word of `prev` and the first of `next` are halves of one word, not two words. */
  private def splitWord(prev: String, next: String, vocab: Set[String]): Boolean =
    (words(prev).lastOption, words(next).headOption) match
      case (Some(last), Some(first)) => prev.toLowerCase.endsWith(last) && vocab(last + first) && !(vocab(last) && vocab(first))
      case _                         => false

  def normalize(item: String): String = item.trim.toLowerCase.replaceAll("\\s+", " ")

  /** Comma or newline separated list -> corrected, deduplicated items. Unknown words get a trailing "?". */
  def items(text: String, vocab: Set[String]): List[String] =
    text.split("[,\n]").toList.map(normalize).filter(_.nonEmpty)
      .map(item => Word.replaceAllIn(item, m => fix(m.matched, vocab).getOrElse(m.matched + "?")))
      .distinct

  /** The word itself if known, else the closest vocabulary word within a length-dependent distance. */
  def fix(word: String, vocab: Set[String]): Option[String] =
    val max = word.length match
      case n if n < 4 => 0
      case n if n < 6 => 1
      case n if n < 9 => 2
      case _          => 3
    if vocab(word) then Some(word)
    else vocab.toList.sorted.map(v => v -> distance(word, v)).filter(_._2 <= max).minByOption(_._2).map(_._1)

  // Levenshtein distance, one row of the table at a time
  private def distance(a: String, b: String): Int =
    a.foldLeft(List.range(0, b.length + 1)) { (prev, ca) =>
      b.toList.zip(prev.zip(prev.tail)).scanLeft(prev.head + 1) { case (left, (cb, (diag, up))) =>
        (up + 1) min (left + 1) min (diag + (if ca == cb then 0 else 1))
      }
    }.last

  case class Section(name: String, items: List[String])

  /** Fruit and vegetables bought by Cristina at the market: always the first section, sent as its own message. */
  val Cristina = "Cristina"

  /** cristina.txt is one item per line; reparti.txt is "[Section]" lines each followed by its items. */
  def parseSections(cristina: String, reparti: String): Either[String, List[Section]] =
    def sections(lines: List[String]): Either[String, List[Section]] = lines match
      case Nil => Right(Nil)
      case s"[$name]" :: rest =>
        val (items, next) = rest.span(!_.startsWith("["))
        sections(next).map(Section(name.trim, items.map(normalize)) :: _)
      case item :: _ => Left(s"reparti.txt: '$item' comes before any [Section]")
    sections(entries(reparti)).map(Section(Cristina, entries(cristina).map(normalize)) :: _)

  def showCristina(sections: List[Section]): String = sections.head.items.mkString("\n")

  def showReparti(sections: List[Section]): String = sections.tail.map(s => (s"[${s.name}]" :: s.items).mkString("\n")).mkString("\n\n")

  /** Items grouped by section, in section order, matching whole items; unmatched ones last under "Da classificare". */
  def sort(items: List[String], sections: List[Section]): List[(String, List[String])] =
    val grouped = items.groupBy(i => sections.find(_.items.contains(normalize(i))).map(_.name))
    sections.flatMap(s => grouped.get(Some(s.name)).map(s.name -> _.sorted)) ++ grouped.get(None).map("Da classificare" -> _.sorted)
