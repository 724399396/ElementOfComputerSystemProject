object JackAnalyzer {
  import java.io.File

  def main(args: Array[String]): Unit = {
    val inFile = new File(args(0))
    val files =
      if (inFile.isDirectory())
        inFile.listFiles().filter(_.getName.endsWith(".jack"))
      else Array(inFile)

    for (file <- files) {
      val out = new File(file.getParentFile(), file.getName.takeWhile(_ != '.') + "_.vm")
      CompilationEngine.recursiveDescentParser(JackTokennizer.tokennizer(file), out);
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
    val ignoreCommentStr: List[String] = ignoreComment(con)

    val sourceString: List[String] = ignoreCommentStr.flatten.zipWithIndex.filter { case (x, _) => x == '"' }.map { case (_, i) => i }.grouped(2).map { case List(s, e) => ignoreCommentStr.flatten.drop(s).take(e - s + 1).mkString }.toList

    var sourceStringLoc = -1
    val res = ignoreCommentStr.flatMap(x => x.replaceAll("\\(", " ( ").
      replaceAll("\\)", " ) ").replaceAll(";", " ; ").
      replaceAll(",", " , ").replaceAll("\\.", " . ").
      replaceAll("~", " ~ ").replaceAll("-", " - ").replaceAll("\\[", " [ ").replaceAll("\\]", " ] ").split("\\s+").toList.foldLeft((List[String](), List[String]())) {
        case ((acc, strCon), x) =>
          if (x.contains("\""))
            if (strCon.isEmpty)
              (acc, strCon ++ List(x))
            else {
              sourceStringLoc += 1
              (acc ++ List(sourceString.apply(sourceStringLoc).mkString("")) /*((strCon ++ List(x)).mkString("")*/ , List[String]())
            }
          else if (strCon.isEmpty)
            (acc ++ List(x), strCon)
          else
            (acc, strCon ++ List(x))

      }._1)
    res
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
  var className = ""
  def recursiveDescentParser(tokens: List[String], outFile: File) = {
    val (res, left) = compileClass(tokens)
    val out = new java.io.PrintWriter(outFile)
    out.print(res)
    out.close()
  }

  def compileClass(str: List[String]): (String, List[String]) = {
    str match {
      case "class" :: x :: "{" :: xs =>
        className = x
        fieldCount = 0
        SymbolTable.startClass()
        val (res1, left1) = compileClassVarDec(xs)
        println(SymbolTable.classTable)
        val (res2, left2) = compileSubroutine(left1)
        left2 match {
          case "}" :: left =>
            (res1 + res2, left)
          case x => println(x); throw new Error("class error 2")
        }
      case x => println(x); throw new Error("class error 1")
    }
  }

  def commaProcess(s: List[String], t: String, k: SymbolTable.Kind, seg: VmWriter.Segment, i: Int = 0): (String, Int, List[String]) = {
    s match {
      case "," :: x :: xs =>
        SymbolTable.define(x, t, k)
        val (ns, j, nl) = commaProcess(xs, t, k, seg, i)
        (ns, j + 1, nl)
      case ";" :: xs =>
        ("", i, xs)
      case _ => ("", i, s)
    }
  }

  var fieldCount = 0

  def compileClassVarDec(str: List[String]): (String, List[String]) = {
    str match {
      case "static" :: x :: y :: xs =>
        SymbolTable.define(y, x, SymbolTable.Static)
        val (c, _, left) = commaProcess(xs, x, SymbolTable.Static, VmWriter.Static)
        val (res, left1) = compileClassVarDec(left)
        (c + res, left1)
      case "field" :: x :: y :: xs =>
        SymbolTable.define(y, x, SymbolTable.Field)
        fieldCount += 1
        val (c, i, left) = commaProcess(xs, x, SymbolTable.Field, VmWriter.Local)
        fieldCount += i
        val (res, left1) = compileClassVarDec(left)
        (res, left1)
      case x => ("", x)
    }
  }

  def compileSubroutine(str: List[String]): (String, List[String]) = {
    val set = Set("constructor", "function", "method")
    SymbolTable.startSubroutine()
    str match {
      case x :: t :: n :: "(" :: xs if set.contains(x) =>
        val prefix = x match {
          case "constructor" =>
            VmWriter.writePush(VmWriter.Constant, fieldCount) + VmWriter.writeCall("Memory.alloc", 1) + VmWriter.writePop(VmWriter.Pointer, 0)
          case "method" =>
            SymbolTable.define("this", "", SymbolTable.Arg)
            VmWriter.writePush(VmWriter.Argument, 0) + VmWriter.writePop(VmWriter.Pointer, 0)
          case _ => ""
        }

        val (res1, left1) = compileParameterList(xs)
        left1 match {
          case ")" :: ys =>
            val (res2, i, left2) = compileSubroutineBody(ys)
            val (res3, left3) = compileSubroutine(left2)
            (VmWriter.writeFunction(className + "." + n, i) + res1 + prefix + res2 + res3, left3)
          case x => println(x); throw new Error("subroutine error")
        }
      case x => ("", x)
    }
  }

  def compileSubroutineBody(str: List[String]): (String, Int, List[String]) = {
    str match {
      case "{" :: ys =>
        val (res2, i, left2) = compileVarDec(ys)
        val (res3, left3) = compileStatements(left2, true)
        left3 match {
          case "}" :: left =>
            (res2 + res3, i, left)
          case x => println(x); throw new Error("subroutineBody error 2")
        }
      case x => println(x); throw new Error("subroutineBody error 1")
    }
  }

  def compileParameterList(str: List[String]): (String, List[String]) = {
    str match {
      case ")" :: xs =>
        ("", str)
      case "," :: xs =>
        val (res, left) = compileParameterList(xs)
        (res, left)
      case t :: n :: xs =>
        SymbolTable.define(n, t, SymbolTable.Arg)
        val (res, left) = compileParameterList(xs)
        (res, left)
      case x =>
        println(x); throw new Error("parameter list error")
    }
  }

  def compileVarDec(str: List[String], i: Int = 0): (String, Int, List[String]) = {
    str match {
      case "var" :: t :: x :: xs =>
        SymbolTable.define(x, t, SymbolTable.Var)
        val (res, j, left) = commaProcess(xs, t, SymbolTable.Var, VmWriter.Local, i + 1)
        left match {
          case y :: ys =>
            val (res2, k, left2) =
              if (y == "var")
                compileVarDec(y :: ys, j)
              else
                ("", j, y :: ys)
            (res + res2, k, left2)
          case x => println(x); throw new Error("var error")
        }
      case x =>
        ("", i, x)
    }
  }

  var ifCur = 0
  var whileCur = 0

  def compileStatements(str: List[String], firstIn: Boolean = false): (String, List[String]) = {
    if (firstIn) {
      ifCur = 0
      whileCur = 0
    }
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
        val (res2, left2) = compileStatements(ys)
        (res1 + res2, left2)
    }
    (res, left)
  }

  def compileDo(str: List[String]): (String, List[String]) = {
    str match {
      case "do" :: n1 :: "." :: n2 :: "(" :: xs =>
        val (res1, i, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ";" :: ys =>
            ((if (n1.head.isUpper) res1 + VmWriter.writeCall(n1 + "." + n2, i) else
               writePushN(n1) + res1 + VmWriter.writeCall(SymbolTable.typeOf(n1).get + "." + n2, i + 1)) +
              VmWriter.writePop(VmWriter.Temp, 0), ys)
          case x => println(x); throw new Error("do error 1")
        }
      case "do" :: n :: "(" :: xs =>
        val (res1, i, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ";" :: ys =>
            (VmWriter.writePush(VmWriter.Pointer, 0) + res1 + VmWriter.writeCall(className + "." + n, i + 1) +
              VmWriter.writePop(VmWriter.Temp, 0), ys)
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
                (res1 + writePushN(n) + VmWriter.writeArithmetic("+") + res2 + VmWriter.writePop(VmWriter.Temp, 0) + VmWriter.writePop(VmWriter.Pointer, 1) + VmWriter.writePush(VmWriter.Temp, 0) + VmWriter.writePop(VmWriter.That, 0), zs)
              case x => println(x); throw new Error("let error 4")
            }
          case x => println(x); throw new Error("let error 3")
        }
      case "let" :: n :: "=" :: xs =>
        val (res1, left1) = compileExpression(xs)
        left1 match {
          case ";" :: ys =>
            (res1 + VmWriter.writePop(kind2seg(SymbolTable.kindOf(n).get), SymbolTable.indexOf(n).get), ys)
          case x => println(x); throw new Error("let error 3")
        }
      case x => println(x); throw new Error("let error")
    }
  }

  def kind2seg(kind: SymbolTable.Kind): VmWriter.Segment = {
    kind match {
      case SymbolTable.Static => VmWriter.Static
      case SymbolTable.Field => VmWriter.This
      case SymbolTable.Arg => VmWriter.Argument
      case SymbolTable.Var => VmWriter.Local
    }
  }

  def writePushN(n: String): String = {
    VmWriter.writePush(kind2seg(SymbolTable.kindOf(n).get), SymbolTable.indexOf(n).get)
  }

  def not(): String = {
    VmWriter.writeArithmetic("~")
  }

  def compileWhile(str: List[String]): (String, List[String]) = {
    val cur = whileCur
    whileCur += 1
    str match {
      case "while" :: "(" :: xs =>
        val (res1, left1) = compileExpression(xs)
        left1 match {
          case ")" :: "{" :: ys =>
            val (res2, left2) = compileStatements(ys)
            left2 match {
              case "}" :: zs =>
                (VmWriter.writeLable(s"WHILE_EXP$cur") + res1 + not()
                  + VmWriter.writeIf(s"WHILE_END$cur") + res2 + VmWriter.writeGoto(s"WHILE_EXP$cur") + VmWriter.writeLable(s"WHILE_END$cur"), zs)
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
        (VmWriter.writePush(VmWriter.Constant, 0) + VmWriter.writeReturn(), xs)
      case "return" :: xs =>
        val (res, left) = compileExpression(xs)
        left match {
          case ";" :: ys =>
            (res + VmWriter.writeReturn(), ys)
          case x => println(x); throw new Error("return error 2")
        }
      case x => println(x); throw new Error("return error")
    }
  }

  def compileIf(str: List[String]): (String, List[String]) = {
    val cur = ifCur
    ifCur += 1
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
                    (res1 + VmWriter.writeIf(s"IF_TRUE$cur") +
                      VmWriter.writeGoto(s"IF_FALSE$cur") + VmWriter.writeLable(s"IF_TRUE$cur") + res2 + VmWriter.writeGoto(s"IF_END$cur") + VmWriter.writeLable(s"IF_FALSE$cur") + res3 +
                      VmWriter.writeLable(s"IF_END$cur"), ks)
                  case x => println(x); throw new Error("if error 4")
                }
              case "}" :: zs =>
                (res1 + VmWriter.writeIf(s"IF_TRUE$cur") + VmWriter.writeGoto(s"IF_FALSE$cur") + VmWriter.writeLable(s"IF_TRUE$cur") + res2 +
                  VmWriter.writeLable(s"IF_FALSE$cur"), zs)
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
        (res + res2 + VmWriter.writeArithmetic(x), left2)
      case x =>
        ("", x)
    }
  }

  def compileExpression(str: List[String]): (String, List[String]) = {
    val (res1, left1) = compileTerm(str, true)
    val (res2, left2) = opProcess(left1)
    (res1 + res2, left2)
  }

  def stringProcess(src: String): String = {
    val x = src.drop(1).dropRight(1)
    VmWriter.writePush(VmWriter.Constant, x.length) + VmWriter.writeCall("String.new", 1) + (for (one <- x) yield { VmWriter.writePush(VmWriter.Constant, one.toInt) + VmWriter.writeCall("String.appendChar", 2) }).mkString
  }

  def compileTerm(str: List[String], start: Boolean = false): (String, List[String]) = {
    str match {
      case x :: xs =>
        JackTokennizer.tokenType(x) match {
          case JackTokennizer.IntegerConstant =>
            (VmWriter.writePush(VmWriter.Constant, x.toInt), xs)
          case JackTokennizer.StringConstant =>
            (stringProcess(x), xs)
          case JackTokennizer.Keyword =>
            (VmWriter.writeKeyWord(x), xs)
          case JackTokennizer.Identifier =>
            xs match {
              case "[" :: ys =>
                val (res1, left1) = compileExpression(ys)
                left1 match {
                  case "]" :: zs =>
                    (res1 + writePushN(x) + VmWriter.writeArithmetic("+") +
                      VmWriter.writePop(VmWriter.Pointer, 1) + VmWriter.writePush(VmWriter.That, 0), zs)
                  case x => println(x); throw new Error("term error")
                }
              case "." :: ys =>
                compileSubroutineCall(str)
              case _ =>
                (writePushN(x), xs)
            }
          case JackTokennizer.Symbol =>
            str match {
              case "-" :: ys =>
                val (res1, left1) = compileTerm(ys)
                (res1 + VmWriter.writeArithmetic(if (start) "neg" else "-"), left1)
              case "~" :: ys =>
                val (res1, left1) = compileTerm(ys)
                (res1 + VmWriter.writeArithmetic("~"), left1)
              case ")" :: ys =>
                ("", str)
              case "(" :: xs =>
                val (res1, left1) = compileExpression(xs)
                left1 match {
                  case ")" :: left2 =>
                    (res1, left2)
                  case x => println(x); throw new Error("term error 4")
                }
              case x =>
                println(x); throw new Error("term error 2")
            }
        }
      case _ =>
        throw new Error("term error 3")
    }
  }

  def compileSubroutineCall(str: List[String]): (String, List[String]) = {
    str match {
      case n1 :: "." :: n2 :: "(" :: xs =>
        val (res1, i, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ys =>
            (if (n1.head.isUpper) res1 + VmWriter.writeCall(n1 + "." + n2, i) else  writePushN(n1) + res1 + VmWriter.writeCall(SymbolTable.typeOf(n1).get + "." + n2, i + 1), ys)
          case x => println(x); throw new Error("subroutine call error 2")
        }
      case n :: "(" :: xs =>
        val (res1, i, left1) = compileExpressionList(xs)
        left1 match {
          case ")" :: ys =>
            ( /*help(n) + help("(") + res1 + help(")") + "</subroutineCall>\n"*/ "", ys)
          case x => println(x); throw new Error("subroutine call error 3")
        }
      case x => println(x); throw new Error("subroutine call error")
    }
  }

  def exComma(str: List[String], i: Int): (String, Int, List[String]) = {
    str match {
      case "," :: xs =>
        val (res1, left1) = compileExpression(xs)
        val (res2, ni, left2) = exComma(left1, i + 1)
        (res1 + res2, ni, left2)
      case x => ("", i, x)
    }
  }

  def compileExpressionList(str: List[String], i: Int = 0): (String, Int, List[String]) = {
    str match {
      case Nil => ("", i, Nil)
      case x =>
        val (res1, left1) = compileExpression(x)
        val (res2, ni, left2) = if (res1 != "") exComma(left1, i + 1) else exComma(left1, 0)
        (res1 + res2, ni, left2)
    }
  }
}

object SymbolTable {
  sealed trait Kind
  case object Static extends Kind
  case object Field extends Kind
  case object Arg extends Kind
  case object Var extends Kind

  import scala.collection.mutable.{LinkedHashMap => MutableMap}
  val classTable = MutableMap[String, (String, Kind, Int)]()
  val subroutineTable = MutableMap[String, (String, Kind, Int)]()

  def startSubroutine(): Unit = {
    subroutineTable.clear()
  }

  def startClass(): Unit = {
    classTable.clear()
  }

  def define(name: String, t: String, kind: Kind): Unit = {
    if (kind == Static || kind == Field) {
      classTable += Tuple2(name, (t, kind, varCount(kind)))
    } else {
      val hash = subroutineTable.size
      subroutineTable += Tuple2(name, (t, kind, varCount(kind)))
    }
  }

  def varCount(kind: Kind): Int = {
    if (kind == Static || kind == Field)
      classTable.filter { case (_, (_, k, _)) => k == kind }.size
    else
      subroutineTable.filter { case (_, (_, k, _)) => k == kind }.size
  }

  def kindOf(name: String): Option[Kind] = {
    classTable.get(name).map { case (_, k, _) => k }.orElse(
      subroutineTable.get(name).map { case (_, k, _) => k }
    )
  }

  def typeOf(name: String): Option[String] = {
    classTable.get(name).map { case (t, _, _) => t }.orElse(
      subroutineTable.get(name).map { case (t, _, _) => t }
    )
  }

  def indexOf(name: String): Option[Int] = {
    classTable.get(name).map { case (_, _, i) => i }.orElse(
      subroutineTable.get(name).map { case (_, _, i) => i }
    )
  }
}

object VmWriter {
  sealed trait Segment
  case object Constant extends Segment
  case object Argument extends Segment
  case object Local extends Segment
  case object Static extends Segment
  case object This extends Segment
  case object That extends Segment
  case object Pointer extends Segment
  case object Temp extends Segment

  import java.io.File
  def codeGen(outFile: File): Unit = {
    val out = new java.io.PrintWriter(outFile)

    out.close
  }

  def writePush(seg: Segment, index: Int): String = {
    s"push ${seg.toString.toLowerCase} $index\n"
  }

  def writePop(seg: Segment, index: Int): String = {
    s"pop ${seg.toString.toLowerCase} $index\n"
  }

  def writeArithmetic(command: String): String = {
    val x = command match {
      case "+" => "add"
      case "-" => "sub"
      case "*" => "call Math.multiply 2"
      case "/" => "call Math.divide 2"
      case "~" => "not"
      case ">" => "gt"
      case "<" => "lt"
      case "=" => "eq"
      case "&" => "and"
      case "neg" => "neg"
      case "|" => "or"
      case _ => throw new Error("unkown arithemetic: " + command)
    }
    x + "\n"
  }

  def writeLable(label: String): String = {
    s"label $label\n"
  }

  def writeGoto(label: String): String = {
    s"goto $label\n"
  }

  def writeIf(label: String): String = {
    s"if-goto $label\n"
  }

  def writeCall(name: String, nArgs: Int): String = {
    s"call $name $nArgs\n"
  }

  def writeFunction(name: String, nLocals: Int): String = {
    s"function $name $nLocals\n"
  }

  def writeReturn(): String = {
    "return\n"
  }

  def writeKeyWord(command: String): String = {
    command match {
      case "true" =>
        writePush(Constant, 0) +
          "not\n"
      case "false" =>
        writePush(Constant, 0)
      case "this" =>
        writePush(Pointer, 0)
      case _ =>
        println("ignore keyword" + command)
        ""
    }
  }
}
