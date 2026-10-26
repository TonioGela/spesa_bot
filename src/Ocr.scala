import cats.effect.{IO, Resource}
import cats.syntax.all.*
import fs2.Stream
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.util.chaining.*

@extern object tess:
  def TessBaseAPICreate(): Ptr[Byte] = extern
  def TessBaseAPIInit3(api: Ptr[Byte], datapath: CString, lang: CString): CInt = extern
  def TessBaseAPISetImage2(api: Ptr[Byte], pix: Ptr[Byte]): Unit = extern
  def TessBaseAPIGetUTF8Text(api: Ptr[Byte]): CString = extern
  def TessDeleteText(text: CString): Unit = extern
  def TessBaseAPIEnd(api: Ptr[Byte]): Unit = extern
  def TessBaseAPIDelete(api: Ptr[Byte]): Unit = extern
  def pixReadMem(data: Ptr[Byte], size: CSize): Ptr[Byte] = extern
  def pixDestroy(pix: Ptr[Ptr[Byte]]): Unit = extern

/** OCR an encoded image (PNG, JPEG, ...). Language data comes from TESSDATA_PREFIX. */
def ocr(image: Stream[IO, Byte], lang: String = "eng+ita"): IO[String] =
  image.compile.to(Array).flatMap { bytes =>
    (pix(bytes), tesseract(lang)).tupled.use { (pix, api) =>
      IO.blocking(tess.TessBaseAPISetImage2(api, pix)) *>
        Resource.make(IO.blocking(tess.TessBaseAPIGetUTF8Text(api)))(t => IO.blocking(tess.TessDeleteText(t)))
          .use(t => IO(fromCString(t)))
    }
  }

/** A decoded Leptonica image; the GC doesn't move objects, so the array can be passed by pointer. */
private def pix(bytes: Array[Byte]): Resource[IO, Ptr[Byte]] =
  Resource.make(
    IO.blocking(if bytes.isEmpty then null else tess.pixReadMem(bytes.at(0), bytes.length.toCSize))
      .flatTap(p => IO.raiseWhen(p == null)(IllegalArgumentException("cannot decode image")))
  )(p => IO.blocking(Zone(tess.pixDestroy(alloc[Ptr[Byte]]().tap(!_ = p))))) // the C API takes Pix**

private def tesseract(lang: String): Resource[IO, Ptr[Byte]] =
  Resource.make(IO.blocking(tess.TessBaseAPICreate()))(api => IO.blocking { tess.TessBaseAPIEnd(api); tess.TessBaseAPIDelete(api) })
    .evalTap(api => IO.blocking(Zone(tess.TessBaseAPIInit3(api, null, toCString(lang))))
      .flatMap(rc => IO.raiseWhen(rc != 0)(IllegalStateException(s"tesseract init failed for '$lang' (TESSDATA_PREFIX?)"))))
