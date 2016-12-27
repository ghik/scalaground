/**
  * Created by ghik on 03.08.16.
  */
package object spanish {
  implicit class StringOps(str: String) {
    private def colored(code: Int) = s"\u001B[${code}m" + str + "\u001B[0m"
    def black = colored(30)
    def red = colored(31)
    def green = colored(32)
    def yellow = colored(33)
    def blue = colored(34)
    def magenta = colored(35)
    def cyan = colored(36)
    def white = colored(37)
  }
}
