import cats.effect.unsafe.implicits.global
import fs2.io.file.{Files, Path}
import cats.effect.IO
import scala.io.Source.fromFile

class SpesaTest extends munit.FunSuite:
  // `scala-cli test` does not embed resources on native, so read the files directly
  val v = Spesa.entries(fromFile("resources/vocabulary.txt").mkString).toSet
  val s = Spesa.parseSections(fromFile("resources/cristina.txt").mkString, fromFile("resources/reparti.txt").mkString).fold(sys.error, identity)

  // splitting a list into items

  test("items are split on commas and newlines"):
    assertEquals(Spesa.items("uova,latte\nburro, pane", v), List("uova", "latte", "burro", "pane"))

  test("items are trimmed, lowercased and inner spaces collapsed"):
    assertEquals(Spesa.items("  Carne   Trita ,\tPasta\t Sfoglia ", v), List("carne trita", "pasta sfoglia"))

  test("empty entries are dropped"):
    assertEquals(Spesa.items("uova,, ,\n\n,latte,", v), List("uova", "latte"))

  test("duplicates are dropped, first occurrence wins"):
    assertEquals(Spesa.items("uova, latte, Uova, uova", v), List("uova", "latte"))

  test("numbers and punctuation inside an item are kept"):
    assertEquals(Spesa.items("pasta sfoglia 2, c'è pane", v), List("pasta sfoglia 2", "c'è pane"))

  test("misspellings are fixed, also when the fix makes a duplicate"):
    assertEquals(Spesa.items("ricottaq, cedano,pasata di pomodoro, Presemulo, panchetto, ricotta", v),
      List("ricotta", "sedano", "passata di pomodoro", "prezzemolo", "pancetta"))

  test("known words are never changed, unknown ones are marked"):
    assertEquals(Spesa.items("pesce, xyzzy", v), List("pesce", "xyzzy?"))

  // rebuilding items from OCR lines

  test("lines without a trailing comma continue the same item"):
    assertEquals(Spesa.fromOcr("carne\ntrita,fettine di\nvitello", v).items, List("carne trita", "fettine di vitello"))

  test("a trailing hyphen joins the next line without a space"):
    assertEquals(Spesa.fromOcr("formag-\ngio ,prosciut-\nto cotto", v).items, List("formaggio", "prosciutto cotto"))

  test("a word split across lines without hyphen is rejoined when the halves are not words"):
    assertEquals(Spesa.fromOcr("peperoni,li\nmone,pasata", v).items, List("peperoni", "limone", "passata"))

  test("two known words across lines stay two words"):
    assertEquals(Spesa.fromOcr("pasta\nsfoglia", v).items, List("pasta sfoglia"))

  test("OCR'd pipes are read as l"):
    assertEquals(Spesa.fromOcr("|imone, que||o", v).items, List("limone", "quello"))

  test("lines without known words are set aside, not lost"):
    val r = Spesa.fromOcr("09:02 \\ all T @@\nformag-\ngio, li\nmone,carne\ntrita\n= ¢ 0O G Z", v)
    assertEquals(r.items, List("formaggio", "limone", "carne trita"))
    assertEquals(r.ignored, List("09:02 \\ all T @@", "= ¢ 0O G Z"))

  test("an empty screenshot gives no items"):
    assertEquals(Spesa.fromOcr("", v), Spesa.Result(Nil, Nil))

  // splitting items into Cristina and the supermarket sections

  test("items match sections only as a whole"):
    assertEquals(Spesa.sort(List("salsa pomodoro", "Pasta  Sfoglia", "pomodoro", "salsa di pomodoro"), s),
      List("Cristina" -> List("pomodoro"), "Frigo" -> List("Pasta  Sfoglia"), "Conserve" -> List("salsa pomodoro"),
        "Da classificare" -> List("salsa di pomodoro")))

  test("sections come in file order, items alphabetical inside them, empty sections omitted"):
    assertEquals(Spesa.sort(List("uova", "pesce", "burro", "zucchine", "carne trita"), s).map(_._1), List("Cristina", "Carne", "Pesce", "Frigo"))
    assertEquals(Spesa.sort(List("uova", "burro"), s), List("Frigo" -> List("burro", "uova")))

  test("section files ignore comments and blank lines and round-trip"):
    assertEquals(Spesa.parseSections("# c\nzucca\n\n", "# r\n[Forno]\npane\n\n[Casa]\nguanti"),
      Right(List(Spesa.Section("Cristina", List("zucca")), Spesa.Section("Forno", List("pane")), Spesa.Section("Casa", List("guanti")))))
    assertEquals(Spesa.parseSections(Spesa.showCristina(s), Spesa.showReparti(s)), Right(s))

  test("items before any section are rejected"):
    assert(Spesa.parseSections("", "pane\n[Forno]").isLeft)

  // the whole pipeline on real screenshots

  def screenshot(name: String) = ocr(Files[IO].readAll(Path(s"test/$name.jpg"))).map(Spesa.fromOcr(_, v)).unsafeToFuture()

  test("screenshot with status bar"):
    screenshot("5978530249551057616").map { r =>
      assertEquals(r.items, List("spinaci", "zucca", "broccoli", "carne trita", "fettine di vitello", "formaggio", "pasta sfoglia 2",
        "verdure", "mozzarella", "patate", "uova", "prosciutto cotto", "funghi", "ricotta", "insalata", "verdure mista",
        "pomodorini", "fagiolini", "parmigiano", "pesce"))
      assertEquals(r.ignored.length, 3)
    }(using munitExecutionContext)

  test("screenshot with words split by hyphens and misspellings"):
    screenshot("5978530249551057617").map { r =>
      assertEquals(r.items, List("peperone", "carote", "broccoli", "sedano", "formaggio zucchine", "mozzarella grattugiata",
        "pasta sfoglia", "parmigiano", "uova", "melanzana", "scamorza", "prosciutto cotto", "detergente pavimento e finito"))
      assertEquals(r.ignored, Nil)
    }(using munitExecutionContext)
