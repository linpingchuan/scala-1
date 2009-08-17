/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.io

import java.io.{ InputStream, Reader, BufferedReader, InputStreamReader, IOException }
import java.nio.charset.{ Charset, CharsetDecoder, CodingErrorAction, CharacterCodingException, MalformedInputException }
import java.nio.channels.Channels
import Source._

object BufferedSource
{  
  /** Reads data from <code>inputStream</code> with a buffered reader,
   *  using encoding in implicit parameter <code>codec</code>.
   * 
   *  @param  inputStream  the input stream from which to read
   *  @param  bufferSize   buffer size (defaults to Source.DefaultBufSize)
   *  @param  reset        a () => Source which resets the stream (if unset, reset() will throw an Exception)
   *  @param  codec        (implicit) a scala.io.Codec specifying behavior (defaults to Codec.default)
   *  @return              the buffered source
   */
  def fromInputStream(
    inputStream: InputStream,
    bufferSize: Int = DefaultBufSize,
    reset: () => Source = null,
    close: () => Unit = null
  )(implicit codec: Codec = Codec.default) =
  {
    new BufferedSource(inputStream, bufferSize, codec) .
      withReset (reset) .
      withClose (close)
  }
}

/** This object provides convenience methods to create an iterable
 *  representation of a source file.
 *
 *  @author  Burak Emir
 *  @version 1.0, 19/08/2004
 */
class BufferedSource(
  inputStream: InputStream,
  bufferSize: Int,
  codec: Codec)
extends Source
{  
  val decoder = codec.decoder
  decoder.reset
  decoder onMalformedInput codec.malformedAction
  val reader = new BufferedReader(new InputStreamReader(inputStream, decoder), bufferSize)
  
  override val iter = new Iterator[Char] {
    private def getc(): Int = 
      try     { reader.read() }
      catch   { case e: CharacterCodingException => codec receivedMalformedInput e }
        
    private[this] var buf_char = getc
    def peek = buf_char
    def hasNext = { buf_char != -1 }
    def next = {
      val c = buf_char.toChar
      buf_char = getc
      c
    }
  }  
}

