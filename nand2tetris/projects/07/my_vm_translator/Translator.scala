//object Main extends App {
  import scala.io.Source

  sealed trait CommandType
  object Arithmetic extends CommandType
  object Push extends CommandType
  object Pop extends CommandType
  object Lable extends CommandType
  object Goto extends CommandType
  object If extends CommandType
  object Function extends CommandType
  object Return extends CommandType
  object Call extends CommandType

  val con = Source.fromFile(args(0)).getLines.toList

  val ignoreBlankAndComment = con.foldLeft((List[String](), false)) {
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
        else
          (acc ++ List(x), comment)
      }
  }._1

  var currentLoc = 0

  val totalLength = ignoreBlankAndComment.size

  def hasMoreCommands(): Boolean = currentLoc < totalLength

  def advance(): Unit = currentLoc += 1

  def commandType(): CommandType = {
    ignoreBlankAndComment(currentLoc) match {
      case "add" => Arithmetic
      case "sub" => Arithmetic
      case "neg" => Arithmetic
      case "eq" => Arithmetic
      case "gt" => Arithmetic
      case "lt" => Arithmetic
      case "and" => Arithmetic
      case "or" => Arithmetic
      case "not" => Arithmetic
      case x if x.contains("push") => Push
      case x if x.contains("pop") => Pop
      case x if x.contains("label") => Lable
      case x if x.contains("goto") => Goto
      case x if x.contains("if-goto") => If
      case x if x.contains("function") => Function
      case x if x.contains("call") => Call
      case x if x.contains("return") => Return
    }
  }

  def arg1(): String = {
    commandType() match {
      case Return =>
        throw new Error("get arg1 from return")
      case _ =>
        ignoreBlankAndComment(currentLoc).trim.split("\\s+") match {
          case Array(x) => x
          case Array(_, x, _*) => x
        }
    }
  }

  def arg2(): Int = {
    def get2(): Int = {
      ignoreBlankAndComment(currentLoc).trim.split("\\s+").apply(2).toInt
    }
    commandType() match {
      case Push =>
        get2()
      case Pop =>
        get2()
      case Function =>
        get2()
      case Call =>
        get2()
      case x =>
        throw new Error(s"get arg2 from $x")
    }
  }

  var out = new java.io.PrintWriter(args(0).takeWhile(_ != '.') + ".asm")

  def setFileName(fileName: String): Unit = {
    out = new java.io.PrintWriter(fileName.takeWhile(_ != '.') + ".asm")
  }

  def push2D(): String = {
    "@SP\nM=M-1;\nA=M;\nD=M;\n"
  }

  def pop2M(): String =
     "@SP\nM=M-1;\nA=M;\n"

  def pushUseD(): String =
     "@SP\nA=M;\nM=D;\n@SP\nM=M+1;\n"

  var judgeTime = 0

  def judge(judgeType: String): String = {
    judgeTime += 1
    s"@b1$judgeTime\nD;$judgeType\n@b2$judgeTime\n0;JMP\n(b1$judgeTime)\n@-1\nD=A;\n@end$judgeTime\n0;JMP\n(b2$judgeTime)\n@0\nD=A;\n@end$judgeTime\n0;JMP\n(end$judgeTime)\n"

  }

  def writeArithmetic(command: String): Unit = {
    val assembly = command match {
      case "add" =>
         pop2M + "D=M;\n" + pop2M + "D=M+D;\n" + pushUseD
      case "sub" =>
        pop2M + "D=M;\n" + pop2M + "D=M-D;\n" + pushUseD
      case "neg" =>
        pop2M + "D=-M;\n" + pushUseD
      case "eq" =>
        pop2M + "D=M;\n" + pop2M + "D=M-D;\n" + judge("JEQ") + pushUseD
      case "gt" =>
        pop2M + "D=M;\n" + pop2M + "D=M-D;\n" + judge("JGT") + pushUseD
      case "lt" =>
        pop2M + "D=M;\n" + pop2M + "D=M-D;\n" + judge("JLT") + pushUseD
      case "and" =>
        pop2M + "D=M;\n" + pop2M + "D=M&D;\n" + pushUseD
      case "or" =>
        pop2M + "D=M;\n" + pop2M + "D=M|D;\n" + pushUseD
      case "not" =>
        pop2M + "D=!M;\n" + pushUseD
    }
    out.print(assembly)
    out.flush()
  }



  def writePushPop(command: String, sgement: String, index: Int): Unit = {
    val assembly = command match {
      case "push" =>
        sgement match {
          case "constant" =>
            s"@$index\nD=A;\n@SP\nA=M;\nM=D;\nD=A+1;\n@SP\nM=D;\n"
        }
    }
    out.print(assembly)
    out.flush()
  }

  def close(): Unit = out.close()
  
  while(hasMoreCommands()) {
    commandType match {
      case Arithmetic =>
        writeArithmetic(arg1())
      case Pop =>
        writePushPop("pop", arg1(), arg2())
      case Push =>
        writePushPop("push", arg1(), arg2())
    }
    advance()
  }
  close()
//}
