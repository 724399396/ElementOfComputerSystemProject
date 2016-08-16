import scala.io.Source

sealed trait CommandType
case object Arithemetic extends CommandType
case object Push extends CommandType
case object Pop extends CommandType

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

while (hasMoreCommands()) {
  commandType() match {
    case Arithemetic =>
      writeArithmetic(arg1)
    case Pop =>
      writePushPop("pop", arg1, arg2)
    case Push =>
      writePushPop("push", arg1, arg2)
  }
  advance()
}

out.close
