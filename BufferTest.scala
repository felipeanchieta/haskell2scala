/* Buffer testing object */

object BufferTest {

  def printInputter() = print("> ")

  def main(args : Array[String]) : Unit = {
    printInputter()
    val line = Console.readLine
    println(line)
  }
}
