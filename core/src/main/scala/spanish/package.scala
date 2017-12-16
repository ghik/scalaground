/**
  * Created by ghik on 03.08.16.
  */
package object spanish {
  implicit class StringOps(str: String) {
    private def colored(code: Int) = s"\u001B[${code}m${str}\u001B[0m"
    def black: String = colored(30)
    def red: String = colored(31)
    def green: String = colored(32)
    def yellow: String = colored(33)
    def blue: String = colored(34)
    def magenta: String = colored(35)
    def cyan: String = colored(36)
    def white: String = colored(37)
  }
}
