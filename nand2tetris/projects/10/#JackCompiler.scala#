object JackAnalyzer {
  import java.io.File

  def main(args: Array[String]): Unit = {
    val inFile = new File(args(0))
    val files = if (inFile.isDirectory())
      inFile.listFiles().filter(_.getName.endsWith(".jack"))
    else Array(inFile)

    for (file <- files) {

      JackTokennizer.run(file)
      //val out = new File(file.getParentFile(), file.getName.takeWhile(_ != '.') + "_.xml")
      //CompilationEngine.recursiveDescentParser(JackTokennizer.tokennizer(file), out);
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

  def specialCharacterAddSpace(str: String, c: String): String = {
    str.split(c).mkString(s""" $c """)
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

  def compileClass(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileClassVarDec(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileSubroutine(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileParameterList(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileVarDec(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileStatements(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileDo(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileLet(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileWhile(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileReturn(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileIf(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileExpression(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileTerm(str: List[String]): (String, List[String]) = {
    ???
  }

  def compileExpressionList(str: List[String]): (String, List[String]) = {
    ???
  }
}
