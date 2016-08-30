object JackAnalyzer {
  import java.io.File

  def main(args: Array[String]): Unit = {
    val inFile = new File(args(0))
    val files = if (inFile.isDirectory())
      inFile.listFiles().filter(_.getName.endsWith(".jack"))
    else Array(inFile)

    for (file <- files) {

      if (args(1) == "parse") {
        val out = new File(file.getParentFile(), file.getName.takeWhile(_ != '.') + "_.xml")
        CompilationEngine.recursiveDescentParser(JackTokennizer.tokennizer(file), out);
      } else {
        JackTokennizer.run(file)
      }

    }
  }
}

object JackTokennizer {
  sealed trait TokenType
  case object Keyword extends TokenType
  case object Symbol extends TokenType
  case object Identifier extends TokenType
  case object IntegerConstant extends TokenType
  case object StringConstant extends TokenType

  sealed trait Keyword
  case object False extends Keyword
  case object While extends Keyword
  case object Function extends Keyword
  case object This extends Keyword
  case object Boolean extends Keyword
  case object If extends Keyword
  case object Class extends Keyword
  case object Do extends Keyword
  case object Null extends Keyword
  case object Char extends Keyword
  case object Void extends Keyword
  case object Return extends Keyword
  case object Constructor extends Keyword
  case object Static extends Keyword
  case object Field extends Keyword
  case object Method extends Keyword
  case object Var extends Keyword
  case object Let extends Keyword
  case object Else extends Keyword
  case object True extends Keyword
  case object Int extends Keyword

  import java.io.File
  import scala.io.Source
  def tokennizer(in: File): List[String] = {
    val con = Source.fromFile(in).getLines.toList
    val ignoreCommentStr = ignoreComment(con)
    ignoreCommentStr.flatMap(x => x.replaceAll("\\(", " ( ").
      replaceAll("\\)", " ) ").replaceAll(";", " ; ").
      replaceAll(",", " , ").replaceAll("\\.", " . ").
      replaceAll("~", " ~ ").replaceAll("\\[", " [ ").replaceAll("\\]", " ] ").split("\\s+").toList.foldLeft((List[String](), List[String]())) {
        case ((acc, strCon), x) =>
          if (x.contains("\""))
            if (strCon.isEmpty)
              (acc, strCon ++ List(x))
            else
              (acc ++ List((strCon ++ List(x)).mkString("")), List[String]())
          else if (strCon.isEmpty)
            (acc ++ List(x), strCon)
          else
            (acc, strCon ++ List(x))

      }._1)
  }

  def run(in: File): Unit = {
    val out = new java.io.PrintWriter(new File(in.getParentFile(), in.getName.takeWhile(_ != '.') + "T_.xml"))
    val tokens = tokennizer(in)
    out.println("<tokens>")
    for (token <- tokens) {
      val t = tokenType(token)
      val content = t match {
        case Keyword => token
        case Symbol => symbol(token)
        case Identifier => identity(token)
        case IntegerConstant => intVal(token)
        case StringConstant => stringVal(token)
      }
      out.println(s"<${t.toString.head.toLower + t.toString.tail}>$content</${t.toString.head.toLower + t.toString.tail}>")
    }
    out.println("</tokens>")
    out.close
  }

  def tokenType(sStr: String): TokenType = {
    val str = sStr.trim
    val keywordSet = Set("class", "constructor", "function",
      "method", "field", "static", "var",
      "int", "char", "boolean", "void", "true",
      "false", "null", "this", "let", "do",
      "if", "else", "while", "return")
    val symbolSet = Set("{", "}", "(", ")", "[", "]", ".",
      ",", ";", "+", "-", "*", "/", "&",
      "|", "<", ">", "=", "~")
    if (keywordSet.contains(str))
      Keyword
    else if (symbolSet.contains(str))
      Symbol
    else if (str.forall(_.isDigit))
      IntegerConstant
    else if (str.startsWith("\"") && str.endsWith("\""))
      StringConstant
    else if (!str.head.isDigit)
      Identifier
    else
      throw new Error("illegal tokenType " + str)
  }

  def keyWord(sStr: String): Keyword = {
    val str = sStr.trim
    str match {
      case "while" => While
      case "class" => Class
      case "false" => False
      case "constructor" => Constructor
      case "true" => True
      case "this" => This
      case "function" => Function
      case "field" => Field
      case "null" => Null
      case "boolean" => Boolean
      case "else" => Else
      case "static" => Static
      case "let" => Let
      case "do" => Do
      case "char" => Char
      case "void" => Void
      case "if" => If
      case "return" => Return
      case "int" => Int
      case "method" => Method
      case "var" => Var
    }
  }

  def symbol(str: String): String = {
    // only when token type is Symbol
    str.head match {
      case '<' => "&lt;"
      case '>' => "&gt;"
      case '"' => "&quot;"
      case '&' => "&amp;"
      case x => x.toString
    }
  }

  def identifier(str: String): String = {
    // only when token type is Identifier
    str
  }

  def intVal(str: String): Int = {

    str.toInt
  }

  def stringVal(str: String): String = {
    str.trim.drop(1).dropRight(1)
  }

  def ignoreComment(str: List[String]): List[String] = str.foldLeft((List[String](), false)) {
    case ((acc, comment), x) =>
      if (comment) {
        if (!x.contains("*/"))
          (acc, comment)
        else
          (acc, false)
      } else {
        if (x.trim.isEmpty || x.startsWith("//"))
          (acc, comment)
        else if (x.trim.startsWith("/*")) {
          if (x.contains("*/"))
            (acc, false)
          else
            (acc, true)
        } else {
          val trimAfer = if (x.indexOf("//") > 0) x.substring(0, x.indexOf("//")).trim else x.trim
          if (trimAfer.isEmpty())
            (acc, comment)
          else
            (acc ++ List(trimAfer), comment)
        }
      }
  }._1
}

object CompilationEngine {
  import java.io.File
  def recursiveDescentParser(tokens: List[String], outFile: File) {
    val (res, left) = compileClass(tokens)
    val out = new java.io.PrintWriter(outFile)
    out.print(res)
    out.close()
  }

  def help(s: String): String = {
    val t = JackTokennizer.tokenType(s)
    s"<${t.toString.head.toLower + t.toString.tail}>${change(s, t)}</${t.toString.head.toLower + t.toString.tail}>\n"
  }

  def change(s: String, t: JackTokennizer.TokenType): String = {
    t match {
      case JackTokennizer.StringConstant => s.drop(1).dropRight(1)
      case JackTokennizer.Symbol =>
        s.head match {
          case '<' => "&lt;"
          case '>' => "&gt;"
          case '"' => "&quot;"
          case '&' => "&amp;"
          case x => x.toString
        }
      case _ => s
    }
  }

  def compileClass(str: List[String]): (String, List[String]) = {
    str match {
      case "class" :: x :: "{" :: xs =>
        val (res1, left1) = compileClassVarDec(xs)
        val (res2, left2) = compileSubroutine(left1)
        left2 match {
          case "}" :: left =>
            ("<class>\n" + help("class") + help(x) + help("{") + res1 + res2 + help("}") + "</class>", left)
          case x => println(x); throw new Error("class error 2")
        }
      case x => println(x); throw new Error("class error 1")
    }
  }

  def commaProcess(s: List[String]): (String, List[String]) = {
    s match {
      case "," :: x :: xs =>
        val (ns, nl) = commaProcess(xs)
        (help(",") + help(x) + ns, nl)
      case ";" :: xs =>
        (help(";"), xs)
      case _ => ("", s)
    }
  }

  def compileClassVarDec(str: List[String]): (String, List[String]) = {
    str match {
      case "static" :: x :: y :: xs =>
        val (c, left) = commaProcess(xs)
        val (res, left1) = compileClassVarDec(left)
        ("<classVarDec>\n" + help("static") + help(x) + help(y) + c + "</classVarDec>\n" + res, left1)
      case "field" :: x :: y :: xs =>
        val (c, left) = commaProcess(xs)
        val (res, left1) = compileClassVarDec(left)
        ("<classVarDec>\n" + help("field") + help(x) + help(y) + c + "</classVarDec>\n" + res, left1)
      case x => ("", x)
    }
  }

  def compileSubroutine(str: List[String]): (String, List[String]) = {
    val set = Set("constructor", "function", "method")
    str match {
      case x :: t :: n :: "(" :: xs if set.contains(x) =>
        val (res1, left1) = compileParameterList(xs)
        left1 match {
          case ")" :: ys =>
            val (res2, left2) = compileSubroutineBody(ys)
            val (res3, left3) = compileSubroutine(left2)
            ("<subroutineDec>\n" + help(x) + help(t) + help(n) + help("(") + res1 + help(")") + res2 + "</subroutineDec>\n" + res3, left3)
          case x => println(x); throw new Error("subroutine error")
        }
      case x => ("", x)
    }
  }

  def compileSubroutineBody(str: List[String]): (String, List[String]) = {
    str match {
      case "{" :: ys =>
        val (res2, left2) = compileVarDec(ys)
        val (res3, left3) = compileStatements(left2)
        left3 match {
          case "}" :: left =>
            ("<subroutineBody>\n" + help("{") + res2 + res3 + help("}") + "</subroutineBody>\n", left)
          case x => println(x); throw new Error("subroutineBody error 2")
        }
      case x => println(x); throw new Error("subroutineBody error 1")
    }
  }

  def compileParameterList(str: List[String]): (String, List[String]) = {
    val (res, left) = compileParameterListHelp(str)
    ("<parameterList>\n" + res + "</parameterList>\n", left)
  }

  def compileParameterListHelp(str: List[String]): (String, List[String]) = {
    str match {
      case ")" :: xs =>
        ("", str)
      case "," :: xs =>
        val (res, left) = compileParameterListHelp(xs)
        (help(",") + res, left)
      case t :: n :: xs =>
        val (res, left) = compileParameterListHelp(xs)
        (help(t) + help(n) + res, left)
      case x =>
        println(x); throw new Error("parameter list error")
    }
  }

  def compileVarDec(str: List[String]): (String, List[String]) = {
    str match {
      case "var" :: t :: x :: xs =>
        val (res, left) = commaProcess(xs)
        left match {
          case y :: ys =>
            val (res2, left2) =
              if (y == "var")
                compileVarDec(y :: ys)
              else
                ("", y :: ys)
            ("<varDec>\n" + help("var") + help(t) + help(x) + res + "</varDec>\n" + res2, left2)
          case x => println(x); throw new Error("var error")
        }
      case x =>
        ("", x)
    }
  }

  def compileStatements(str: List[String], firstIn: Boolean = true): (String, List[String]) = {
    val (res1, left1) = str match {
      case "let" :: xs =>
        compileLet(str)
      case "if" :: xs =>
        compileIf(str)
      case "while" :: xs =>
        compileWhile(str)
      case "do" :: xs =>
        compileDo(str)
      case "return" :: xs =>
        compileReturn(str)
      case x => println(x); throw new Error("statements error")
    }
    val (res, left) = left1 match {
      case "}" :: xs =>
        (res1, left1)
      case ys =>
        val (res2, left2) = compileStatements(ys, false)
        (res1 + res2, left2)
    }
    (if (firstIn) ("<statements>\n") + res + ("</statements>\n") else res, left)
  }

  def compileDo(str: List[String]): (String, List[String]) = {
    str match {
      case "do" :: n1 :: "." :: n2 :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ";" :: ys =>
            ("<doStatement>\n" + help("do") + help(n1) + help(".") + help(n2) + help("(") + res1 + help(")") + help(";") + "</doStatement>\n", ys)
          case x => println(x); throw new Error("do error 1")
        }
      case "do" :: n :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ";" :: ys =>
            ("<doStatement>\n" + help("do") + help(n) + help("(") + res1 + help(")") + help(";") + "</doStatement>\n", ys)
          case x => println(x); throw new Error("do error 2")
        }

      case x => ("", x)
    }
  }

  def compileLet(str: List[String]): (String, List[String]) = {
    //'let' varName ( '[' expression ']' )? '=' expression ';'
    str match {
      case "let" :: n :: "[" :: xs =>
        val (res1, left1) = compileExpression(xs)
        left1 match {
          case "]" :: "=" :: ys =>
            val (res2, left2) = compileExpression(ys)
            left2 match {
              case ";" :: zs =>
                ("<letStatement>\n" + help("let") + help(n) + help("[") + res1 + help("]") + help("=") + res2 + help(";") + "</letStatement>\n", zs)
              case x => println(x); throw new Error("let error 4")
            }
          case x => println(x); throw new Error("let error 3")
        }
      case "let" :: n :: "=" :: xs =>
        val (res1, left1) = compileExpression(xs)
        left1 match {
          case ";" :: ys =>
            ("<letStatement>\n" + help("let") + help(n) + help("=") + res1 + help(";") + "</letStatement>\n", ys)
          case x => println(x); throw new Error("let error 3")
        }
      case x => println(x); throw new Error("let error")
    }
  }

  def compileWhile(str: List[String]): (String, List[String]) = {
    str match {
      case "while" :: "(" :: xs =>
        val (res1, left1) = compileExpression(xs)
        left1 match {
          case ")" :: "{" :: ys =>
            val (res2, left2) = compileStatements(ys)
            left2 match {
              case "}" :: zs =>
                ("<whileStatement>\n" + help("while") + help("(") +
                  res1 + help(")") + help("{") + res2 + help("}") +
                  "</whileStatement>\n", zs)
              case x => println(x); throw new Error("while error 3")
            }
          case x => println(x); throw new Error("while error 2")
        }
      case x => println(x); throw new Error("while error")
    }
  }

  def compileReturn(str: List[String]): (String, List[String]) = {
    str match {
      case "return" :: ";" :: xs =>
        ("<returnStatement>\n" + help("return") + help(";") + "</returnStatement>\n", xs)
      case "return" :: xs =>
        val (res, left) = compileExpression(xs)
        left match {
          case ";" :: ys =>
            ("<returnStatement>\n" + help("return") + res + help(";") + "</returnStatement>\n", ys)
          case x => println(x); throw new Error("return error 2")
        }
      case x => println(x); throw new Error("return error")
    }
  }

  def compileIf(str: List[String]): (String, List[String]) = {
    str match {
      case "if" :: "(" :: xs =>
        val (res1, left1) = compileExpression(xs)
        left1 match {
          case ")" :: "{" :: ys =>
            val (res2, left2) = compileStatements(ys)
            left2 match {
              case "}" :: "else" :: "{" :: zs =>
                val (res3, left3) = compileStatements(zs)
                left3 match {
                  case "}" :: ks =>
                    ("<ifStatement>\n" + help("if") + help("(") + res1 + help(")") + help("{") + res2 + help("}") + help("else") + help("{") + res3 + help("}") +
                      "</ifStatement>\n", ks)
                  case x => println(x); throw new Error("if error 4")
                }
              case "}" :: zs =>
                ("<ifStatement>\n" + help("if") + help("(") + res1 + help(")") + help("{") + res2 + help("}") + "</ifStatement>\n", zs)
              case x => println(x); throw new Error("if error 3")
            }
          case x => println(x); throw new Error("if error 2")
        }
      case x => println(x); throw new Error("if error 1")
    }
  }

  def opProcess(str: List[String]): (String, List[String]) = {
    val set = Set("+", "-", "*", "/", "&", "|", "<", ">", "=")
    str match {
      case x :: xs if set.contains(x) =>
        val (res, left) = compileTerm(xs)
        val (res2, left2) = opProcess(left)
        (help(x) + res + res2, left2)
      case x =>
        ("", x)
    }
  }

  def compileExpression(str: List[String]): (String, List[String]) = {
    val (res1, left1) = compileTerm(str)
    val (res2, left2) = opProcess(left1)

    (if (res1.isEmpty) res1 + res2 else "<expression>\n" + res1 + res2 + "</expression>\n", left2)
  }

  def compileTerm(str: List[String]): (String, List[String]) = {
    val (res, left) = str match {
      case x :: xs =>
        JackTokennizer.tokenType(x) match {
          case JackTokennizer.IntegerConstant =>
            (help(x), xs)
          case JackTokennizer.StringConstant =>
            (help(x), xs)
          case JackTokennizer.Keyword =>
            (help(x), xs)
          case JackTokennizer.Identifier =>
            xs match {
              case "[" :: ys =>
                val (res1, left1) = compileExpression(ys)
                left1 match {
                  case "]" :: zs =>
                    (help(x) + help("[") + res1 + help("]"), zs)
                  case x => println(x); throw new Error("term error")
                }
              case "." :: ys =>
                compileSubroutineCall(str)
              case _ =>
                (help(x), xs)
            }
          case JackTokennizer.Symbol =>
            str match {
              case "-" :: ys =>
                val (res1, left1) = compileTerm(ys)
                (help("-") + res1, left1)
              case "~" :: ys =>
                val (res1, left1) = compileTerm(ys)
                (help("~") + res1, left1)
              case ")" :: ys =>
                ("", str)
              case "(" :: xs =>
                val (res1, left1) = compileExpression(xs)
                left1 match {
                  case ")" :: left2 =>
                    (help("(") + res1 + help(")"), left2)
                  case x => println(x); throw new Error("term error 4")
                }
              case x =>
                println(x); throw new Error("term error 2")
            }
        }
      case _ =>
        throw new Error("term error 3")
    }
    (if (res.isEmpty) res else "<term>\n" + res + "</term>\n", left)
  }

  def compileSubroutineCall(str: List[String]): (String, List[String]) = {
    str match {
      case n1 :: "." :: n2 :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ys =>
            (help(n1) + help(".") + help(n2) + help("(") + res1 + help(")"), ys)
          case x => println(x); throw new Error("subroutine call error 2")
        }
      case n :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ys =>
            (help(n) + help("(") + res1 + help(")") + "</subroutineCall>\n", ys)
          case x => println(x); throw new Error("subroutine call error 3")
        }
      case x => println(x); throw new Error("subroutine call error")
    }
  }

  def exComma(str: List[String]): (String, List[String]) = {
    str match {
      case "," :: xs =>
        val (res1, left1) = compileExpression(xs)
        val (res2, left2) = exComma(left1)
        (help(",") + res1 + res2, left2)
      case x => ("", x)
    }
  }

  def compileExpressionList(str: List[String]): (String, List[String]) = {
    str match {
      case Nil => ("", Nil)
      case x =>
        val (res1, left1) = compileExpression(x)
        val (res2, left2) = exComma(left1)
        ("<expressionList>\n" + res1 + res2 + "</expressionList>\n", left2)
    }
  }
}
