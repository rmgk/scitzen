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

/** Utility methods related to validating and escaping XML; used internally but
  * potentially useful outside of Scalatags.
  */
object Escaping {
  /** Code to escape text HTML nodes. Based on code from scala.xml */
  def escape(text: String, s: java.io.Writer) = {
    // Implemented per XML spec:
    // http://www.w3.org/International/questions/qa-controls
    // Highly imperative code, ~2-3x faster than the previous implementation (2020-06-11)
    val charsArray = text.toCharArray
    val len        = charsArray.size
    var pos        = 0
    var i          = 0
    while (i < len) {
      val c = charsArray(i)
      c match {
        case '<' =>
          s.write(charsArray, pos, i - pos)
          s.write("&lt;")
          pos = i + 1
        case '>' =>
          s.write(charsArray, pos, i - pos)
          s.write("&gt;")
          pos = i + 1
        case '&' =>
          s.write(charsArray, pos, i - pos)
          s.write("&amp;")
          pos = i + 1
        case '"' =>
          s.write(charsArray, pos, i - pos)
          s.write("&quot;")
          pos = i + 1
        case '\n' =>
        case '\r' =>
        case '\t' =>
        case c if c < ' ' =>
          s.write(charsArray, pos, i - pos)
          pos = i + 1
        case _ =>
      }
      i += 1
    }
    // Apparently this isn't technically necessary if (len - pos) == 0 as
    // it doesn't cause any exception to occur in the JVM.
    // The problem is that it isn't documented anywhere so I left this if here
    // to make the error clear.
    if (pos < len) {
      s.write(charsArray, pos, len - pos)
    }
  }
}
