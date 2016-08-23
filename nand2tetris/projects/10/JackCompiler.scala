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
    s"<${t.toString.head.toLower + t.toString.tail}>$s</${t.toString.head.toLower + t.toString.tail}>"
  }

  def compileClass(str: List[String]): (String, List[String]) = {
    str match {
      case "class" :: x :: "{" :: xs =>
        val (res1, left1) = compileClassVarDec(xs)
        val (res2, left2) = compileSubroutine(left1)
        left2 match {
          case "}" :: left =>
            ("<class>" + help("</class>") + help(x) + help("{") + res1 + res2 + help("}") + "</class>", left)
        }
    }
  }

  def commaProcess(s: List[String]): (String, List[String]) = {
    s match {
      case "," :: x :: xs =>
        val (ns, nl) = commaProcess(xs)
        (help(",") + help(x) + ns, nl)
      case _ => ("", s)
    }
  }

  def compileClassVarDec(str: List[String]): (String, List[String]) = {
    str match {
      case "static" :: x :: y :: xs =>
        val (c, left) = commaProcess(xs)
        (help("<classVarDec>") + help("static") + help(x) + help(y) + c + help("</classVarDec>"), left)
      case "field" :: x :: y :: xs =>
        val (c, left) = commaProcess(xs)
        (help("<classVarDec>") + help("static") + help(x) + help(y) + c + help("</classVarDec>"), left)
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
            ("<subroutineDec>" + help(x) + help(t) + help(n) + help("(") + res1 + help(")") + res2 + "</subroutineDec>", left2)
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
            ("<subroutineBody>" + help("{") + res2 + res3 + help("}") + "</subroutineBody>", left)
        }
    }
  }

  def compileParameterList(str: List[String]): (String, List[String]) = {
    val (res, left) = str match {
      case t :: n :: xs if JackTokennizer.tokenType(t) == JackTokennizer.Identifier =>
        val (res, left) = commaProcess(xs)
        (help(t) + help(n) + res, left)
      case x =>
        ("", x)
    }
    ("<parameterList>" + res + "</parameterList>", left)
  }

  def compileVarDec(str: List[String]): (String, List[String]) = {
    str match {
      case "var" :: t :: x :: xs =>
        val (res, left) = commaProcess(xs)
        left match {
          case ";" :: y :: ys =>
            val (res2, left2) =
              if (y == "var")
                compileVarDec(y :: ys)
              else
                ("", y :: ys)
            ("<varDec>" + help("var") + help(t) + help(x) + res + help(";") + "</varDec>" + res2, left2)
        }
      case x =>
        ("", x)
    }
  }

  def compileStatements(str: List[String]): (String, List[String]) = {
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
    }
    val (res, left) = left1 match {
      case "}" :: xs =>
        (res1, left1)
      case ys =>
        val (res2, left2) = compileStatements(ys)
        (res1 + res2, left2)
    }
    (help("<statements>") + res + help("</statements>"), left)
  }

  def compileDo(str: List[String]): (String, List[String]) = {
    str match {
       case "do" :: n1 :: "." :: n2 :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ys =>
            ("<doStatement>" + help("do") + help(n1) + help(".") + help(n2) + help("(") + res1 + help(")") + "</doStatement>", ys)
        }
      case "do" :: n :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ys =>
            ("<doStatement>" + help("do") + help(n) + help("(") + res1 + help(")") + "</doStatement>", ys)
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
                 ("<letStatement>" + help("let") + help(n) + help("[") + res1 + help("]") + help("=") + res2 + help(";") + "</letStatement>", zs)
            }
        }
      case "let" :: n :: "=" :: xs =>
        val (res1, left1) = compileExpression(xs)
        left1 match {
          case ";" :: ys =>
            ("<letStatement>" + help("let") + help(n) + help("=") + res1 + help(";") + "</letStatement>", ys)
        }

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
                ("<whileStatement>" + help("while") + help("(") +
                  res1 + help(")") + help("{") + res2 + help("}") +
                  "</whileStatement>", zs)
            }
        }
    }
  }

  def compileReturn(str: List[String]): (String, List[String]) = {
    str match {
      case "return" :: ";" :: xs =>
        ("<returnStatement>" + help("return") + help(";") + "</returnStatement>", xs)
      case "return" :: xs =>
        val (res, left) = compileExpression(xs)
        left match {
          case ";" :: ys =>
            ("<returnStatement>" + help("return") + res + help(";") + "</returnStatement>", ys)
        }
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
                    ("<ifStatement>" + help("if") + help("(") + res1 + help(")") + help("{") + res2 + help("}") + help("else") + help("{") + res3 + help("}") +
                      "</ifStatement>", ks)
              case "}" :: zs =>
                    ("<ifStatement>" + help("if") + help("(") + res1 + help(")") + help("{") + res2 + help("}")  + "</ifStatement>", zs)
            }
        }
    }
  }

  def opProcess(str: List[String]): (String, List[String]) = {
    val set = Set("+", "-", "*", "/", "&", "|", "<", ">", "=")
    str match {
      case x :: xs if set.contains(x) =>
        val (res, left) = compileTerm(xs)
        val (res2, left2) = opProcess(left)
        (help(x) + res2, left2)
      case x =>
        ("", x)
    }
  }

  def compileExpression(str: List[String]): (String, List[String]) = {
    val (res1, left1) = compileTerm(str)
    val (res2, left2) = onProcess(left1)
    ("<expression>" + res + res2 + "</expression>", left2)
  }

  def compileTerm(str: List[String]): (String, List[String]) = {
    val (res, left) = str match {
      case x :: xs =>
        JackTokennizer.tokenType(x) match {
          case JackTokennizer.IntegerConstant =>
            (help(x),xs)
          case JackTokennizer.StringConstant =>
            (help(x),xs)
          case JackTokennizer.Keyword =>
            (help(x),xs)
          case JackTokennizer.Identifier =>
            xs match {
              case "[" :: ys =>
                val (res1, left1) = compileExpression(ys)
                left1 match {
                  case "]" :: zs =>
                    (help(x) + help("[") + res1 + help("]"), zs)
                }
              case "(" :: ys =>
                compileSubroutineCall(str)
              case ys =>
                (help(x),xs)
            }
          case JackTokennizer.Symbol =>
           str match {
             case "(" :: ys =>
               val (res1, left1) = compileExpression(ys)
               left1 match {
                 case ")" :: zs =>
                   (help("(") + res1 + help(")"), zs)
               }
             case "+" :: ys =>
               val (res1, left1) = compileTerm(ys)
               (help("+") + res1, left1)
           }
        }
    }
    ("<term>" + res + "</term>", left)
  }

  def compileSubroutineCall(str: List[String]): (String, List[String]) = {
    str match {
    case n1 :: "." :: n2 :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ys =>
            ("<subroutineCall>" + help(n1) + help(".") + help(n2) + help("(") + res1 + help(")") + "</subroutineCall>", ys)
        }
      case n :: "(" :: xs =>
        val (res1, left1) = compileExpressionList(xs)
 p      left1 match {
          case ")" :: ys =>
            ("<subroutineCall>" + help(n) + help("(") + res1 + help(")") + "</subroutineCall>", ys)
}
  }

  def exComma(str: List[String]): (String, List[String]) = {
    str match {
      case "," :: xs =>
        val (res1, left1) = compileExpression(xs)
        val (res2, left2) = exComma(left1)
        (help(",") + res1 + res2, left2)
    }
  }

  def compileExpressionList(str: List[String]): (String, List[String]) = {
    str match {
      case Nil => ("", Nil)
      case x =>
        val (res1, left1) = compileExpression(x)
        val (res2, left2) = exComma(left1)
        ("<expressionList>" + res1 + res2 + "</expressionList>", left2)
    }
  }
}
