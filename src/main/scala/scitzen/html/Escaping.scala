/*
Copyright (c) 2018 Li Haoyi (haoyi.sg@gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

package scitzen.html

import java.io.OutputStream
import java.nio.charset.StandardCharsets

/** Utility methods related to validating and escaping XML; used internally but
  * potentially useful outside of Scalatags.
  */
object Escaping {

  val `&lt;` = "&lt;".getBytes(StandardCharsets.UTF_8)
  val `&gt;` = "&gt;".getBytes(StandardCharsets.UTF_8)
  val `&amp;` = "&amp;".getBytes(StandardCharsets.UTF_8)
  val `&quot;` = "&quot;".getBytes(StandardCharsets.UTF_8)

  /** Code to escape text HTML nodes. Based on code from scala.xml
    * Adapted to work with byte arrays of UTF8 for less copying.
    */
  def escape(text: Array[Byte], outputStream: OutputStream) = {
    // Implemented per XML spec:
    // http://www.w3.org/International/questions/qa-controls
    val inputSize           = text.size
    var inputLastCopy       = 0
    var inputCheckPos       = 0

    def write(escape: Array[Byte]) =
      val len = inputCheckPos - inputLastCopy
      outputStream.write(text, inputLastCopy, len)
      outputStream.write(escape)
      inputLastCopy = inputCheckPos + 1

    while (inputCheckPos < inputSize) {
      text(inputCheckPos) match {
        case '<'                    => write(`&lt;`)
        case '>'                    => write(`&gt;`)
        case '&'                    => write(`&amp;`)
        case '"'                    => write(`&quot;`)
        case '\n'                   =>
        case '\r'                   =>
        case '\t'                   =>
        case c if c < ' ' && c >= 0 => write(Array.emptyByteArray)
        case other                  =>
      }
      inputCheckPos += 1
    }

    write(Array.emptyByteArray)

  }
}
