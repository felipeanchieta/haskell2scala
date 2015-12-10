/*

    The MIT License (MIT)

    Copyright (c) 2015 Caue Massi de Oliveira, Felipe Anchieta Santos Costa and
Rodrigo Martins de Oliveira

    Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

/* Protects our definition from earlier definitions to conflict using SEALED */
sealed trait List[+A]
case object Nil extends List[Nothing]
/* Technique to divide a list of tokens in head/tail for patt. matching */
case class Cons[+A](head: A, tail: List[A]) extends List[A]
case class Token(t: Symbol, v: String)

object List {
    def invert[A](list: List[A]): List[A] = {
        def go(pending: List[A], inverted: List[A]): List[A] = pending match {
            case Nil => inverted
            case Cons(h,t) => go(t, Cons(h, inverted))
        }
        go(list, Nil)
    }

    def charList(line: String): List[Char] = {
        def go(i: Int, str: String, list: List[Char]): List[Char] = {
            if (i < str.length)
                go(i+1, str, Cons(str(i).toChar,list))
            else
                list
        }
        go(0, line, Nil)
    }

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

// Main object
object Interpreter {

    /* Tokenize(line)
     * Receives a string, break it into tokens and return a list of tokens,
     * classifying each token with a related type, using Symbol, and a value,
     * using String
     */
    def Tokenize(line: String): List[Token] = {

        /* Intern main recursive loop */
        def go(stack: List[Char], tokens: List[Token]): List[Token] = stack match {

            case Nil => tokens
            case Cons(h, t) =>
                /* Join the numerical symbols to form a entire number to be tokenized after */
                if(Character.isDigit(h)) {
                    tokens match {
                        case Cons(Token('INTEGER, v), e) => go(t, Cons(Token('INTEGER,h+v), e))
                        case _ => go(t, Cons(Token('INTEGER,h.toString), tokens))
                    }
                } else if(h == ' ') {
                /* Classify space to later ignore them in Clear() */
                    tokens match {
                        case Cons(Token('SPACE, v), _) => go(t, tokens)
                        case _ => go(t, Cons(Token('SPACE,h.toString), tokens))
                    }
                } else if(h == '(' || h == ')') {
                    go(t, Cons(Token('PARENTHESES,h.toString), tokens))

                } else if (h == '\'') {
                    go(t, Cons(Token('COMMA, h.toString), tokens))
                } else if (h == '\"') {
                    go(t, Cons(Token('DOUBLECOMMA, h.toString), tokens))
                } else if (h == '[' || h == ']') {
                    go(t, Cons(Token('BRACKETS, h.toString), tokens))
                } else if(Character.isLetter(h)) {
                /* Groups alphabetical symbols to form a literal, which can be a
                   reserved word, a label to constant/method etc. */
                    tokens match {
                        case Cons(Token('LITERAL, v), e) => go(t, Cons(Token('LITERAL,h+v), e))
                        case _ => go(t, Cons(Token('LITERAL,h.toString), tokens))
                    }
                } else if(h == '+' || h == '-' || h == '*' || h == '/' || h == '>' || h == '<' || h == '=') {
                    tokens match {
                        case Cons(Token('OPERATOR, v), e) => go(t, Cons(Token('OPERATOR,h+v), e))
                        case _ => go(t, Cons(Token('OPERATOR,h.toString), tokens))
                    }
                } else {
                    /* Escape case, when we don't know the type of the token,
                    purposely or not */
                    tokens match {
                        case Cons(Token('UNKNOWN, v), _) => go(t, tokens)
                        case _ => go(t, Cons(Token('UNKNOWN,h.toString), tokens))
                    }
                }
        }

        def clear(list: List[Token], clean: List[Token]): List[Token] = list match {
            case Cons(Token('SPACE, _), t) => clear(t, clean)
            case Cons(h, t) => clear(t, Cons(h, clean))
            case Nil => clean
        }

        val chars = List.charList(line)
        val tokens = go(chars, Nil)
        clear(tokens, Nil)
    }

    /*  Check(tokens)
     *  Receives a list of tokens, then checks if token types and order matches
     *  and returns the result of the verification, which is boolean
     *  @RODRIGO> Document this section
     */
    def Check(tokens: List[Token]): Boolean = {
        def go(incoming: List[Token], pending: List[Token]): Boolean = {
            def clear(list: List[Token]): List[Token] = list match {
                case Cons(Token('INTEGER, _), Cons(Token('OPERATOR, _), Cons(Token('INTEGER, _), t))) => clear(Cons(Token('INTEGER, ""),t))
                case _ => list
            }
            def jump(list: List[Token]): List[Token] = list match {
                case Cons(Token('PARENTHESES, "("),t) => Cons(Token('INTEGER, ""), t)
                case Cons(h, t) => jump(t)
                case Nil => Nil
            }
            val stack = clear(pending)
            incoming match {
                case Nil => stack match {
                    case Nil => true
                    case Cons(Token('INTEGER, _), Nil) => true
                    case _ => false
                }
                case Cons(Token('UNKNOWN, _), t) => false
                case Cons(Token('PARENTHESES, ")"), t) => go(t, Nil) && go(jump(t),stack)
                case Cons(Token('PARENTHESES, "("), t) => go(Nil, stack)
                case Cons(h, t) => go(t, Cons(h, stack))
            }
        }

        go(tokens, Nil)
    }

    /*  Evaluate()
     *  Receive a list of tokens and evaluate the result of the processing on
     *  the expression, passing, recursively, a List of constants used in the
     *  Haskell program
     */
    def Evaluate(expr: List[Token], map: Map[Symbol, String]): (String, Map[Symbol, String]) = {
        def printTokens(tokens: List[Token]): Unit = tokens match {
            case Nil => print("\n")
            case Cons(Token(_,v), t) => {
                print(v)
                printTokens(t)
            }
        }
        def jump(list: List[Token]): List[Token] = list match {
            case Cons(Token('PARENTHESES, "("),t) => t
            case Cons(h, t) => jump(t)
            case Nil => Nil
        }
        def go(tokens: List[Token], pending: List[Token], goMap: Map[Symbol, String]): (List[Token], Map[Symbol, String]) = {
            /*println("\nTokens")
            println(tokens)
            println("Pending")
            println(pending)*/
            def clear(pending: List[Token]): (List[Token], Map[Symbol, String]) = {
                def process(x: List[Token]): Token = x match {
                    case Cons(Token('INTEGER, a), Cons(Token('OPERATOR, "+"), Cons(Token('INTEGER, b),t))) => Token('INTEGER,(a.toInt + b.toInt).toString)
                    case Cons(Token('INTEGER, a), Cons(Token('OPERATOR, "-"), Cons(Token('INTEGER, b),t))) => Token('INTEGER,(a.toInt - b.toInt).toString)
                    case Cons(Token('INTEGER, a), Cons(Token('OPERATOR, "*"), Cons(Token('INTEGER, b),t))) => Token('INTEGER,(a.toInt * b.toInt).toString)
                    case Cons(Token('INTEGER, a), Cons(Token('OPERATOR, "/"), Cons(Token('INTEGER, b),t))) => Token('INTEGER,(a.toInt / b.toInt).toString)
                    case _ => Token('UNKNOWN, "")
                }
                pending match {
                    case Cons(Token('LITERAL, "let"), Cons(Token('LITERAL, id), Cons(Token('OPERATOR, "="), t))) => {
                        val newMap = goMap + (Symbol(id) -> Evaluate(List.invert(t), goMap)._1)
                        //print("a ")
                        //println(newMap)
                        (Cons(Token('INTEGER, newMap(Symbol(id))), Nil), newMap)
                    }
                    case Cons(Token('LITERAL, a), t) => t match {
                        case Cons(Token('OPERATOR, "="), _) => (pending, goMap)
                        case _ => clear(Cons(Token('INTEGER, goMap(Symbol(a))), t))
                    }
                    /*case Cons(Token('LITERAL, x), Nil) => {
                        //println(goMap)
                        if(goMap.contains(Symbol(x))) (Cons(Token('INTEGER, goMap(Symbol(x))), Nil), goMap)
                        else (Cons(Token('UNKNOWN,""), Nil),goMap)
                    }*/
                    /*case Cons(Token('OPERATOR, a), Cons( Token('LITERAL, b), Cons( Token('OPERATOR, c), Cons(Token('LITERAL, d), t)))) =>
                        if( a == "+" || a == "-") {
                            //println("if")
                            (Cons(Token('OPERATOR, a),Cons(process(Cons(Token('INTEGER, goMap(Symbol(b))), Cons( Token('OPERATOR, c), Cons(Token('INTEGER, goMap(Symbol(b))), Nil)))),t)), goMap)
                        } else {
                            //println("else")
                            (pending, goMap)
                        }*/
                    case Cons(Token('EOL, _), Cons( Token('LITERAL, a), t)) =>
                        clear(Cons(Token('EOL,""), Cons(Token('INTEGER, goMap(Symbol(a))), t)))
                    case Cons(Token('PARENTHESES, "("), t) => clear(Cons(Token('EOL,""), t))
                    case Cons(Token('OPERATOR, a), Cons( Token('INTEGER, b), Cons( Token('OPERATOR, c), Cons(Token('INTEGER, d), t)))) =>
                        if( a == "+" || a == "-") {
                            //println("if")
                            (Cons(Token('OPERATOR, a),Cons(process(Cons(Token('INTEGER, b), Cons( Token('OPERATOR, c), Cons(Token('INTEGER, d), Nil)))),t)), goMap)
                        } else {
                            //println("else")
                            (pending, goMap)
                        }
                    case Cons(Token('EOL, a), Cons( Token('INTEGER, b), Cons( Token('OPERATOR, c), Cons(Token('INTEGER, d), t)))) =>
                        (Cons(process(Cons(Token('INTEGER, b), Cons( Token('OPERATOR, c), Cons(Token('INTEGER, d), Nil)))),t), goMap)
                    case Cons(Token('EOL, _), Cons( Token('INTEGER, a), Nil)) =>
                        (Cons(Token('INTEGER, a), Nil), goMap)
                    case _ => (pending, goMap)
                }
            }
            print("arriving: ")
            printTokens(tokens)
            print("pending: ")
            printTokens(pending)
            tokens match {
                case Nil => clear(Cons(Token('EOL,""), pending))
                case Cons(Token('PARENTHESES,"("),t) => {
                    clear(Cons(Token('EOL,""), pending))
                }
                case Cons(Token('PARENTHESES,")"),t) => {
                    val p = go(t,Nil,goMap)
                    /*print("\t intermediate: ")
                    printTokens(p._1)*/
                    List.invert(p._1) match {
                        case Cons(h, t) => go(Cons(h, jump(tokens)), pending, p._2)
                    }

                }
                case Cons(h, t) => {
                    val p = clear(Cons(h, pending))
                    go(t, p._1, p._2)
                }
            }
        }

        go(expr, Nil, map) match {
            case (Cons(Token('INTEGER, a), Nil), newMap) =>{
                //println(newMap)
                (a, newMap)
            }
            case (x, newMap) => Evaluate(x, newMap)
        }

    }

    /* Read-Evaluate-Print Loop
       Receives the expression from the user, evaluate it and prints the result,
       which can be just the return of the expression or an error, in case of a
       mistyping of the user
     */
    def repl(map: Map[Symbol, String]): Map[Symbol, String] = {

        val line = readLine(">>> ")
        val tokens = Tokenize(line)
        val matching = Check(tokens)

        //if (matching) {
        if (true) {
            val res = Evaluate(tokens, map)
            println(res._1)
            repl(res._2)
        } else {
            println("Syntax error, be careful.\n")
            repl(map)
        }

    }

    def showVersion() {
        println("UFABC Haskell Interpreter. Copyright (c) 2015. This software is"+
        "licensed by MIT License and comes WITHOUT any warranty.\n"+
        "See LICENSE file for further information.")
    }

    def main(args: Array[String]) {
        /* Ignites the map of constants which will storage the constants type by
           the user of the interpreter */
        val map:Map[Symbol,String] = Map()

        if (args.length != 0 && (args(0) == "-v" || args(0) == "--version"))
            showVersion()
        else
            repl(map)
    }
}
