object Interpreter {
  val keyWords : Set[String] = Set (
    "as",
    "case",
    "class",
    "data",
    "data family",
    "data instance",
    "default",
    "deriving",
    "deriving instance",
    "do",
    "else",
    "forall",
    "foreign",
    "hiding",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "mdo",
    "module",
    "newtype",
    "proc",
    "qualified",
    "rec",
    "then",
    "type",
    "type family",
    "type instance",
    "where"
  )

  val specialSymbols : Set[String] = Set (
    "!",
    "\'",
    "\"",
    "-",
    "--",
    "-<",
    "-<<",
    "->",
    ":",
    "::",
    ",",
    "=",
    "=>",
    ">", "<",
    "?",
    "#",
    "*",
    "@",
    "[",
    "|",
    "]",
    "\\",
    "_",
    "`",
    "{",
    "}",
    "{-",
    "-}",
    "~"
  )

  def startParser(expr : String) : Unit = {
    /*  TODO
        1) Get the first character of the expression.
        2) Verifies if the character is the first character is the first of one of the keyWords
          IF IT IS:
            Verify, recursevely, if the second character is the second and goes on
            Verify if the expression is valid. If not, it's a syntax error. Throw new exception.
          IF IT ISN'T:
            Verify if the expression produces a return, the return the return. If not, throw
            a new exception.
        3) Loop recursevely
      */
  }
}
