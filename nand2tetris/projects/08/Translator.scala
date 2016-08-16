object Main extends App {
  import scala.io.Source

  sealed trait CommandType
  case object Arithemetic extends CommandType
  case object Push extends CommandType
  case object Pop extends CommandType
  case object Lable extends CommandType
  case object Goto extends CommandType
  case object If extends CommandType
  case object Function extends CommandType
  case object Return extends CommandType
  case object Call extends CommandType

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

  val totalLength = ignoreBlankAndComment.size

  var currentLoc = 0

  def hasMoreCommands(): Boolean = currentLoc < totalLength

  def advance(): Unit = currentLoc += 1

  def commandType(): CommandType = {
    val arithmetic = Set("add", "sub", "neg", "eq", "gt", "lt", "and", "or", "not")
    val currentCommand = ignoreBlankAndComment(currentLoc)
    if (arithmetic.contains(currentCommand))
      Arithemetic
    else currentCommand match {
      case x if x.startsWith("pop") => Pop
      case x if x.startsWith("push") => Push
      case x if x.startsWith("lable") => Lable
      case x if x.startsWith("goto") => Goto
      case x if x.startsWith("if-goto") => If
      case x if x.startsWith("function") => Function
      case x if x.startsWith("call") => Call
      case x if x.startsWith("return") => Return
    }
  }

  def arg1(): String = {
    val currentCommand = ignoreBlankAndComment(currentLoc)
    currentCommand.split("\\s+") match {
      case Array(x) => x
      case Array(_, x, _*) => x
    }
  }

  def arg2(): Int = {
    val currentCommand = ignoreBlankAndComment(currentLoc)
    currentCommand.split("\\s+").apply(2).toInt
  }

  val out = new java.io.PrintWriter(args(0).takeWhile(_ != '.') + ".asm")

  def pop2M() =
    """@SP
    M=M-1
    @SP
    A=M
  """

  def pushD() =
    """@SP
     A=M
     M=D
     @SP
     M=M+1
  """

  var jumpTime = 0
  def jump(jumpType: String) = {
    jumpTime += 1
    s"""@b$jumpTime
D;$jumpType
@0
D=A
@end$jumpTime
0;JMP
(b$jumpTime)
@0
D=!A
(end$jumpTime)
  """
  }

  def writeArithmetic(command: String): Unit = {
    val assembly = command match {
      case "add" =>
        pop2M + "D=M\n" + pop2M + "D=D+M\n" + pushD
      case "sub" =>
        pop2M + "D=M\n" + pop2M + "D=M-D\n" + pushD
      case "neg" =>
        pop2M + "D=-M\n" + pushD
      case "eq" =>
        pop2M + "D=M\n" + pop2M + "D=M-D\n" + jump("JEQ") + pushD
      case "gt" =>
        pop2M + "D=M\n" + pop2M + "D=M-D\n" + jump("JGT") + pushD
      case "lt" =>
        pop2M + "D=M\n" + pop2M + "D=M-D\n" + jump("JLT") + pushD
      case "and" =>
        pop2M + "D=M\n" + pop2M + "D=D&M\n" + pushD
      case "or" =>
        pop2M + "D=M\n" + pop2M + "D=D|M\n" + pushD
      case "not" =>
        pop2M + "D=!M\n" + pushD
    }
    out.print(assembly.split("\n").map(_.trim).mkString("\n"))
  }

  def pop2a(index: Int, reg: String): String =
    s"""@$index
           D=A
           @$reg
           D=M+D
           @R15
           M=D
           @SP
           AM=M-1
           D=M
           @15
           A=M
           M=D
           """

  def pop2b(index: Int, reg: String): String =
    s"""@$index
           D=A
           @$reg
           D=A+D
           @R15
           M=D
           @SP
           AM=M-1
           D=M
           @15
           A=M
           M=D
           """

  def push2a(index: Int, reg: String): String =
    s"""@$index
           D=A
           @$reg
           A=M+D
           D=M
           @SP
           A=M
           M=D
           @SP
           M=M+1
           """

  def push2b(index: Int, reg: String): String =
    s"""@$index
           D=A
           @$reg
           A=A+D
           D=M
           @SP
           A=M
           M=D
           @SP
           M=M+1
           """

  def writePushPop(command: String, segment: String, index: Int): Unit = {
    val assembly = command match {
      case "push" =>
        segment match {
          case "constant" =>
            s"""@$index
             D=A
""" + pushD()
          case "local" => push2a(index, "LCL")
          case "argument" =>
            push2a(index, "ARG")
          case "this" =>
            push2a(index, "THIS")
          case "that" =>
            push2a(index, "THAT")
          case "temp" =>
            push2b(index, "R5")
          case "pointer" =>
            push2b(index, "THIS")
          case "static" =>
            push2b(index, "16")
        }
      case "pop" =>
        segment match {
          case "local" =>
            pop2a(index, "LCL")
          case "argument" =>
            pop2a(index, "ARG")
          case "this" =>
            pop2a(index, "THIS")
          case "that" =>
            pop2a(index, "THAT")
          case "temp" =>
            pop2b(index, "R5")
          case "pointer" =>
            pop2b(index, "THIS")
          case "static" =>
            pop2b(index, "16")
        }
    }
    out.print(assembly.split("\n").map(_.trim).mkString("\n"))
  }

  def splitWrite(str: String) = {
    out.print(str.split("\n").map(_.trim).mkString("\n"))
  }

  def writeInit(): Unit = {
    val str = """@256
       D=A
       @SP
       M=D
       xx call Sys.init
    """
    splitWrite(str)
  }

  def writeLable(label: String): Unit = {
    splitWrite(s"($label)\n")
  }

  def writeGoto(label: String): Unit = {
    val assembly = """@$lable
                   0;JMP
                   """
    splitWrite(assembly)
  }

  def writeIf(label: String): Unit = {
    val assembly = """@SP
                   D=M
                   @$lable
                   D;JGT
                   """
    splitWrite(assembly)
  }

  def writeCall(functionName: String, numArgs: Int): Unit = {
    val assembly = s"""@${functionName}returnAddress
                   D=A
                   $pushD
                   @LCL
                   D=M
                   $pushD
                   @ARG
                   D=M
                   $pushD
                   @THIS
                   D=M
                   $pushD
                   @THAT
                   D=M
                   $pushD
                   @SP
                   D=M
                   @$numArgs
                   D=D-A
                   @5
                   D=D-5
                   @ARG
                   M=D
                   @SP
                   D=M
                   @LCL
                   M=D
                   """ + writeGoto(functionName) +
                   """(${functionName}returnAddress)
                   """
    splitWrite(assembly)
  }

  def writeReturn(): Unit = {
     val assembly = s"""@LCL
                   D=M
                   @14
                   M=D
                   @5
                   D=D-A
                   @13
                   M=D
                   $pop2M
                   D=M
                   @ARG
                   M=D
                   D=A+1
                   @SP
                   M=D
                   @14
                   D=M
                   @1
                   D=M-A
                   @THAT
                   M=D
                   @THIS
                   @14
                   D=M
                   @2
                   D=M-A
                   @THIS
                   M=D
@14
                   D=M
                   @3
                   D=M-A
                   @ARG
                   M=D
                   @THIS
                   @14
                   D=M
                   @4
                   D=M-A
                   @LCL
                   M=D
                   @13
                   A=M
                   0;JMP
                   """
    splitWrite(assembly)
  }

  def writeFunction(functionName: String, numLocals: Int): Unit = {
    val asemmbly = """($functionName)
                   """ + (0 until numLocals).map("@0\nD=A\n" + pushD()).mkString("\n")
  }

  while (hasMoreCommands()) {
    var f = ""
    commandType() match {
      case Arithemetic =>
        writeArithmetic(arg1)
      case Pop =>
        writePushPop("pop", arg1, arg2)
      case Push =>
        writePushPop("push", arg1, arg2)
      case Lable =>
        writeLable(f + "$" + arg1)
      case Goto =>
        writeGoto(f + "$" + arg1)
      case If =>
        writeIf(f + "$" + arg1)
      case Function =>
        f = arg1
        writeFunction(arg1, arg2)
      case Return =>
        writeReturn()
      case Call =>
        writeCall(arg1, arg2)
    }
    advance()
  }

  out.close
}
