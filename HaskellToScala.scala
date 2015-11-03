object HaskellToScala {

	def showStartText() {
		println("""
				|Santo André Haskell Interpreter (c) 2015
			   	|Written by Felipe Anchieta and Rodrigo Martins.
			   	|Check the LICENSE file before doing anything stupid.
			   	|This software comes WITHOUT ANY WARRANTY.""".stripMargin)
	}

	def showInterpreterLine(str : String) {
		print(str)
	}

	def processWords() : Boolean {
		showInterpreterLine("> ")
		// TODO: Input
		val input = /* scanner.nextLine() ? */

		// TODO parser
		if (startParsing(input) == END) {
			false
		} else {
			true
		}
	}

	def main(args: Array[String]) {
		showStartText()

		if (processWords) {
			processWords
		}
	}
}
