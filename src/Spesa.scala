import cats.syntax.all.*
import scala.io.Source

/** Grocery-list cleanup: drops non-list lines, rejoins split words, fixes spelling, sorts items into sections. */
object Spesa:
  case class Result(items: List[String], ignored: List[String])

  case class Section(name: String, items: List[String])

  /** `fixes` are "wrong -> right" word corrections, applied before guessing the closest known word;
   *  `always` are items added to every list. */
  case class Config(sections: List[Section], fixes: Map[String, String], always: List[String] = Nil):
    /** The known words: every word of every item. */
    lazy val vocabulary: Set[String] = sections.flatMap(_.items).flatMap(words).toSet

  /** Fruit and vegetables bought by Cristina at the market: a section sent as its own message. */
  val Cristina = "Cristina"

  /** The config section holding the word corrections. */
  val Fixes = "Correzioni"

  /** The config section holding the items added to every list. */
  val Always = "Sempre"

  /** Group of the items found in no section. */
  val Missing = "Senza reparto"

  lazy val builtin: String = Source.fromInputStream(getClass.getResourceAsStream("/config.txt")).mkString

  /** config.txt: "[Section]" lines each followed by its items, [Sempre] holding items added to every list,
   *  [Correzioni] holding "wrong -> right" lines;
   *  blank lines and `#` comments are ignored. */
  def parseConfig(text: String): Either[String, Config] =
    def sections(lines: List[String]): Either[String, List[Section]] = lines match
      case Nil => Right(Nil)
      case s"[$name]" :: rest =>
        val (items, next) = rest.span(!_.startsWith("["))
        sections(next).map(Section(name.trim, items.map(normalize)) :: _)
      case item :: _ => Left(s"config.txt: '$item' comes before any [Section]")
    sections(text.linesIterator.map(_.trim).filter(l => l.nonEmpty && !l.startsWith("#")).toList).flatMap { all =>
      val (fixes, rest)      = all.partition(_.name.equalsIgnoreCase(Fixes))
      val (always, sections) = rest.partition(_.name.equalsIgnoreCase(Always))
      fixes.flatMap(_.items).traverse {
        case s"$wrong->$right" if normalize(wrong).nonEmpty && normalize(right).nonEmpty => Right(normalize(wrong) -> normalize(right))
        case line => Left(s"config.txt: '$line' in [$Fixes] is not 'wrong -> right'")
      }.map(fs => Config(sections, fs.toMap, always.flatMap(_.items).distinct))
    }

  private val Word = raw"\p{L}+(?:'\p{L}+)?".r

  def words(text: String): List[String] = Word.findAllIn(text.toLowerCase).toList

  /** Text of an OCR'd screenshot -> items. Lines without any known word (status bar, notes) are set aside. */
  def fromOcr(text: String, config: Config): Result =
    val lines = text.replace('|', 'l').linesIterator.map(_.trim).filter(_.nonEmpty)
    // rejoin words split across lines: "formag-" "gio", "li" "mone"; the latest line is the head
    val merged = lines.foldLeft(List.empty[String]) {
      case (prev :: acc, next) if prev.endsWith("-")                        => (prev.init + next) :: acc
      case (prev :: acc, next) if splitWord(prev, next, config.vocabulary) => (prev + next) :: acc
      case (acc, next)                                                      => next :: acc
    }.reverse
    val (kept, ignored) = merged.partition(l => words(l).exists(w => w.length >= 3 && fix(w, config).isDefined))
    Result(items(kept.mkString(" "), config), ignored)

  /** The last word of `prev` and the first of `next` are halves of one word, not two words. */
  private def splitWord(prev: String, next: String, vocab: Set[String]): Boolean =
    (words(prev).lastOption, words(next).headOption) match
      case (Some(last), Some(first)) => prev.toLowerCase.endsWith(last) && vocab(last + first) && !(vocab(last) && vocab(first))
      case _                         => false

  def normalize(item: String): String = item.trim.toLowerCase.replaceAll("\\s+", " ")

  /** Comma or newline separated list -> corrected, deduplicated items. Unknown words of 3+ letters get a trailing "?". */
  def items(text: String, config: Config): List[String] =
    text.split("[,\n]").toList.map(normalize).filter(_.nonEmpty)
      .map(item => Word.replaceAllIn(item, m => fix(m.matched, config).getOrElse(if m.matched.length < 3 then m.matched else m.matched + "?")))
      .distinct

  /** A corrected list plus the items always added, deduplicated. */
  def list(text: String, config: Config): List[String] = (items(text, config) ++ config.always).distinct

  /** The configured correction, else the word itself if known, else the closest known word within a length-dependent distance. */
  def fix(word: String, config: Config): Option[String] =
    val max = word.length match
      case n if n < 4 => 0
      case n if n < 6 => 1
      case n if n < 9 => 2
      case _          => 3
    config.fixes.get(word)
      .orElse(Option.when(config.vocabulary(word))(word))
      .orElse(config.vocabulary.toList.sorted.map(v => v -> distance(word, v)).filter(_._2 <= max).minByOption(_._2).map(_._1))

  // Levenshtein distance, one row of the table at a time
  private def distance(a: String, b: String): Int =
    a.foldLeft(List.range(0, b.length + 1)) { (prev, ca) =>
      b.toList.zip(prev.zip(prev.tail)).scanLeft(prev.head + 1) { case (left, (cb, (diag, up))) =>
        (up + 1) min (left + 1) min (diag + (if ca == cb then 0 else 1))
      }
    }.last

  /** Items grouped by section, in section order, matching whole items; unmatched ones last under [[Missing]]. */
  def sort(items: List[String], config: Config): List[(String, List[String])] =
    val grouped = items.groupBy(i => config.sections.find(_.items.contains(normalize(i))).map(_.name))
    config.sections.flatMap(s => grouped.get(Some(s.name)).map(s.name -> _.sorted)) ++ grouped.get(None).map(Missing -> _.sorted)
