import cats.effect.{IO, Resource}
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
  def TessBaseAPIDelete(api: Ptr[Byte]): Unit = extern
  def pixReadMem(data: Ptr[Byte], size: CSize): Ptr[Byte] = extern
  def pixDestroy(pix: Ptr[Ptr[Byte]]): Unit = extern

/** OCR an encoded image (PNG, JPEG, ...). Language data comes from TESSDATA_PREFIX. */
def ocr(image: Stream[IO, Byte]): IO[String] =
  image.compile.to(Array).flatMap { bytes =>
    pix(bytes).use { pix =>
      IO.blocking(engine.synchronized {
        tess.TessBaseAPISetImage2(engine.api, pix)
        val t = tess.TessBaseAPIGetUTF8Text(engine.api)
        try fromCString(t) finally tess.TessDeleteText(t)
      })
    }
  }

/** Loading the models takes ~0.6s, so one instance lives for the whole process; it isn't thread-safe, hence the lock in `ocr`. */
private object engine:
  // on failure the lazy val stays unset, so the next image retries
  lazy val api: Ptr[Byte] =
    val a = tess.TessBaseAPICreate()
    if tess.TessBaseAPIInit3(a, null, c"eng+ita") != 0 then
      tess.TessBaseAPIDelete(a)
      throw IllegalStateException("tesseract init failed for 'eng+ita' (TESSDATA_PREFIX?)")
    // freed before exit, otherwise tesseract's own cleanup prints leak warnings
    sys.addShutdownHook(engine.synchronized(tess.TessBaseAPIDelete(a)))
    a

/** A decoded Leptonica image; the GC doesn't move objects, so the array can be passed by pointer. */
private def pix(bytes: Array[Byte]): Resource[IO, Ptr[Byte]] =
  Resource.make(
    IO.blocking(if bytes.isEmpty then null else tess.pixReadMem(bytes.at(0), bytes.length.toCSize))
      .flatTap(p => IO.raiseWhen(p == null)(IllegalArgumentException("cannot decode image")))
  )(p => IO.blocking(Zone(tess.pixDestroy(alloc[Ptr[Byte]]().tap(!_ = p))))) // the C API takes Pix**
