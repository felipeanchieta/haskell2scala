object HaskellToScala {

	def showStartText() {
		println(
			 """|Santo André Haskell Interpreter (c) 2015
			   	|Written by Felipe Anchieta and Rodrigo Martins.
			   	|Check the LICENSE file before doing anything stupid.
			   	|This software comes WITHOUT ANY WARRANTY.""".stripMargin)
	}

	def showInterpreterLine(str : String) : Unit = print(str)

	def processWords() : Boolean = {
		showStartText()
		showInterpreterLine("> ")
		val input = Console.readLine
		true
	}

	def showVersion() {
		println("Santo André Haskell Interpreter (c) 2015. Version 0.1-dev")
		System.exit(0)
	}

	def main(args: Array[String]) : Unit = {

		def processArgs(args : List[String]) : Unit = args match {
			case Nil => processArgs(args.tail)
			case "-v" :: _ | "--version" :: _ => showVersion()
			case _ => println("Don\'t mess with the arguments!")
		}

		if (args.isEmpty) processWords() else processArgs(args.toList)
	}
}
