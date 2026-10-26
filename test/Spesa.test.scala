import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import scala.io.Source.fromFile

class SpesaTest extends munit.FunSuite:
  // `scala-cli test` does not embed resources on native, so read the file directly
  val c = Spesa.parseConfig(fromFile("resources/config.txt").mkString).fold(sys.error, identity)
  val v = c.vocabulary

  // the configuration file

  test("the vocabulary is every word of every item"):
    assert(Set("pasta", "sfoglia", "di", "pomodoro", "zucca").subsetOf(v))
    assert(!v("quello"))

  test("the config ignores comments and blank lines"):
    assertEquals(Spesa.parseConfig("# r\n[Cristina]\nZucca\n\n[Forno]\npane  nero\n[Correzioni]\nPanne -> pane\n"),
      Right(Spesa.Config(List(Spesa.Section("Cristina", List("zucca")), Spesa.Section("Forno", List("pane nero"))), Map("panne" -> "pane"))))

  test("a list corrected by hand is trusted: only [Correzioni] apply, no guesses, leftover marks dropped"):
    val noAlways = c.copy(always = Nil)
    assertEquals(Spesa.list("verdure? miste?, cedano, ricottaa, Verdure Miste", noAlways), List("verdure miste", "sedano", "ricottaa"))
    assertEquals(Spesa.sort(Spesa.list("verdure miste", noAlways), noAlways), List(Spesa.Missing -> List("verdure miste")))

  test("[Sempre] items are added to every list once, and are not a section"):
    val sempre = Spesa.parseConfig("[Frigo]\nuova\nburro\n[Sempre]\nUova\n").fold(sys.error, identity)
    assertEquals(sempre.sections.map(_.name), List("Frigo"))
    assertEquals(Spesa.list("burro, uova", sempre), List("burro", "uova"))
    assertEquals(Spesa.sort(Spesa.list("burro", sempre), sempre), List("Frigo" -> List("burro", "uova")))

  test("items before any section and malformed corrections are rejected"):
    assert(Spesa.parseConfig("pane\n[Forno]").isLeft)
    assert(Spesa.parseConfig("[Correzioni]\npanne").isLeft)

  test("configured corrections win over known words and guesses"):
    val custom = c.copy(fixes = Map("ricotta" -> "mozzarella", "zuca" -> "zucchine"))
    assertEquals(Spesa.items("ricotta, zuca, cedano", custom), List("mozzarella", "zucchine", "sedano"))
    assertEquals(Spesa.items("cedano", c), List("sedano"))

  // splitting a list into items

  test("items are split on commas and newlines"):
    assertEquals(Spesa.items("uova,panna\nburro, pesto", c), List("uova", "panna", "burro", "pesto"))

  test("items are trimmed, lowercased and inner spaces collapsed"):
    assertEquals(Spesa.items("  Carne   Trita ,\tPasta\t Sfoglia ", c), List("carne trita", "pasta sfoglia"))

  test("empty entries are dropped"):
    assertEquals(Spesa.items("uova,, ,\n\n,panna,", c), List("uova", "panna"))

  test("duplicates are dropped, first occurrence wins"):
    assertEquals(Spesa.items("uova, panna, Uova, uova", c), List("uova", "panna"))

  test("numbers and punctuation inside an item are kept"):
    assertEquals(Spesa.items("pasta sfoglia 2, speck/pancetta", c), List("pasta sfoglia 2", "speck/pancetta"))

  test("misspellings are fixed, also when the fix makes a duplicate"):
    assertEquals(Spesa.items("ricottaq, cedano,pasata di pomodoro, Presemulo, panchetto, ricotta", c),
      List("ricotta", "sedano", "passata di pomodoro", "prezzemolo", "pancetta"))

  test("known words are never changed, unknown ones of 3+ letters are marked"):
    assertEquals(Spesa.items("pesce, xyzzy, speck o pancetta", c), List("pesce", "xyzzy?", "speck o pancetta"))

  // rebuilding items from OCR lines

  test("lines without a trailing comma continue the same item"):
    assertEquals(Spesa.fromOcr("carne\ntrita,fettine di\nvitello", c).items, List("carne trita", "fettine di vitello"))

  test("a trailing hyphen joins the next line without a space"):
    assertEquals(Spesa.fromOcr("formag-\ngio ,prosciut-\nto cotto", c).items, List("formaggio", "prosciutto cotto"))

  test("a word split across lines without hyphen is rejoined when the halves are not words"):
    assertEquals(Spesa.fromOcr("peperoni,li\nmone,pasata", c).items, List("peperoni", "limone", "passata"))

  test("two known words across lines stay two words"):
    assertEquals(Spesa.fromOcr("pasta\nsfoglia", c).items, List("pasta sfoglia"))

  test("OCR'd pipes are read as l"):
    assertEquals(Spesa.fromOcr("|imone, mozzare||a", c).items, List("limone", "mozzarella"))

  test("lines without known words are set aside, not lost"):
    val r = Spesa.fromOcr("09:02 \\ all T @@\nformag-\ngio, li\nmone,carne\ntrita\nQuello che c'è no lo prendete\n= ¢ 0O G Z", c)
    assertEquals(r.items, List("formaggio", "limone", "carne trita"))
    assertEquals(r.ignored, List("09:02 \\ all T @@", "Quello che c'è no lo prendete", "= ¢ 0O G Z"))

  test("an empty screenshot gives no items"):
    assertEquals(Spesa.fromOcr("", c), Spesa.Result(Nil, Nil))

  // splitting items into sections

  test("items match sections only as a whole; the rest is reported as missing"):
    assertEquals(Spesa.sort(List("salsa pomodoro", "Pasta  Sfoglia", "pomodoro", "salsa di pomodoro"), c),
      List("Cristina" -> List("pomodoro"), "Frigo" -> List("Pasta  Sfoglia"), "Conserve" -> List("salsa pomodoro"),
        Spesa.Missing -> List("salsa di pomodoro")))

  test("sections come in file order, items alphabetical inside them, empty sections omitted"):
    val shop = Spesa.parseConfig("[Cristina]\nzucchine\n[Carne]\ncarne trita\n[Pesce]\npesce\n[Frigo]\nuova\nburro").fold(sys.error, identity)
    assertEquals(Spesa.sort(List("uova", "pesce", "burro", "zucchine", "carne trita"), shop).map(_._1), List("Cristina", "Carne", "Pesce", "Frigo"))
    assertEquals(Spesa.sort(List("uova", "burro"), shop), List("Frigo" -> List("burro", "uova")))

  // the whole pipeline on real screenshots

  def screenshot(name: String) = ocr(Files[IO].readAll(Path(s"test/$name.jpg"))).map(Spesa.fromOcr(_, c)).unsafeToFuture()

  test("screenshot with status bar"):
    screenshot("5978530249551057616").map { r =>
      assertEquals(r.items, List("spinaci", "zucca", "broccoli", "carne trita", "fettine di vitello", "formaggio", "pasta sfoglia 2",
        "verdure?", "mozzarella", "patate", "uova", "prosciutto cotto", "funghi", "ricotta", "insalata", "verdure? mista?",
        "pomodorini", "fagiolini", "parmigiano", "pesce"))
      assertEquals(r.ignored.length, 3)
    }(using munitExecutionContext)

  test("screenshot with words split by hyphens and misspellings"):
    screenshot("5978530249551057617").map { r =>
      assertEquals(r.items, List("peperoni", "carote", "broccoli", "sedano", "formaggio zucchine", "mozzarella grattugiata",
        "pasta sfoglia", "parmigiano", "uova", "melanzane", "scamorza", "prosciutto cotto", "detergente pavimento e finito?"))
      assertEquals(r.ignored, Nil)
    }(using munitExecutionContext)
