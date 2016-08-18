object JackAnalyzer {
  import java.io.File

  def main(args: Array[String]): Unit = {
    val inFile = new File(args(0))
    val files = if (inFile.isDirectory())
      inFile.listFiles().filter(_.getName.endsWith(".jack"))
    else Array(inFile)

    for (file <- files) {
      val out = new File(file.getParentFile(), file.getName.takeWhile(_ != '.') + "_.xml")
      JackTokennizer.run(file)
      //CompilationEngine.recursiveDescentParser(JackTokennizer.tokennizer(file), out);
    }
  }
}

object JackTokennizer {
  sealed trait TokenType
  case object KeyWord extends TokenType
  case object Symbol extends TokenType
  case object Identifier extends TokenType
  case object IntConst extends TokenType
  case object StringConst extends TokenType

  sealed trait KeyWord
  case object False extends KeyWord
  case object While extends KeyWord
  case object Function extends KeyWord
  case object This extends KeyWord
  case object Boolean extends KeyWord
  case object If extends KeyWord
  case object Class extends KeyWord
  case object Do extends KeyWord
  case object Null extends KeyWord
  case object Char extends KeyWord
  case object Void extends KeyWord
  case object Return extends KeyWord
  case object Constructor extends KeyWord
  case object Static extends KeyWord
  case object Field extends KeyWord
  case object Method extends KeyWord
  case object Var extends KeyWord
  case object Let extends KeyWord
  case object Else extends KeyWord
  case object True extends KeyWord
  case object Int extends KeyWord

  import java.io.File
  import scala.io.Source
  def tokennizer(in: File): List[String] = {
    val con = Source.fromFile(in).getLines.toList
    val ignoreCommentStr = ignoreComment(con)
    ignoreCommentStr.flatMap(x => x.replaceAll("\\(", " ( ").
        replaceAll("\\)", " ) ").replaceAll(";", " ; ").
        replaceAll(",", " , ").replaceAll("\\.", " . ").split("\\s+").toList)
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
        case KeyWord => token
        case Symbol => symbol(token)
        case Identifier => identity(token)
        case IntConst => intVal(token)
        case StringConst => stringVal(token)
      }
      out.println(s"<${t.toString.toLowerCase}>$content</${t.toString.toLowerCase}>")
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
      "|", "<", ">", "=", " ~ ")
    if (keywordSet.contains(str))
      KeyWord
    else if (symbolSet.contains(str))
      Symbol
    else if (str.forall(_.isDigit))
      IntConst
    else if (str.startsWith("\"") && str.endsWith("\""))
      StringConst
    else if (!str.head.isDigit)
      Identifier
    else
      throw new Error("illegal tokenType " + str)
  }

  def keyWord(sStr: String): KeyWord = {
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

  def symbol(str: String): Char = {
    // only when token type is Symbol
    str.head
  }

  def identifier(str: String): String = {
    // only when token type is Identifier
    str
  }

  def intVal(str: String): Int = {

    str.toInt
  }

  def stringVal(str: String): String = {
    str.drop(1).dropRight(1)
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
        else if (x.startsWith("/*"))
          (acc, true)
        else {
          val trimAfer = x.takeWhile(_ != '/').trim
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
