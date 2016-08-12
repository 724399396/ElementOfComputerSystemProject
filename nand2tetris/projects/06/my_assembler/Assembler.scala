import scala.io.Source
// dest=comp;jump

sealed trait CommandType
case object ACommand extends CommandType
case object CCommand extends CommandType
case object LCommand extends CommandType

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
        (acc ++ List(x.filterNot(_ == ' ')), comment)
    }
}._1

import collection.mutable.{Map => MutableMap}
val symbolTable: MutableMap[String, Int] = MutableMap()

def addEntry(s: String, a: Int): Unit = {
  symbolTable += s -> a
}

def contains(s: String): Boolean = symbolTable.contains(s)

def getAddress(s: String): Int = symbolTable(s)

val totalLength = ignoreBlankAndComment.size

var currentLoc = 0

def hasMoreCommands(): Boolean = currentLoc < totalLength

def advance(): Unit = currentLoc += 1

def commandType(): CommandType = {
  val currentCommand = ignoreBlankAndComment(currentLoc)
  if (currentCommand.startsWith("@"))
    ACommand
  else if (currentCommand.startsWith("("))
    LCommand
  else
    CCommand
}

def symbol(): String = {
  commandType match {
    case ACommand =>
      ignoreBlankAndComment(currentLoc).substring(1)
    case LCommand =>
      ignoreBlankAndComment(currentLoc).drop(1).dropRight(1)
    case CCommand =>
      throw new Error("illegal symbol operation")
  }
}

def dest(): String = {
  commandType match {
    case CCommand =>
      val currentCommand =  ignoreBlankAndComment(currentLoc)
      if (currentCommand.contains("="))
        currentCommand.takeWhile(_ != '=')
      else
        "null"
    case _ =>
      throw new Error("illegal symbol operation")
  }
}

def comp(): String = {
  commandType match {
    case CCommand =>
      ignoreBlankAndComment(currentLoc).dropWhile(_ != '=').drop(1).takeWhile(_ != ';')
    case _ =>
      throw new Error("illegal symbol operation")
  }
}

def jump(): String = {
  commandType match {
    case CCommand =>
      val tmp = ignoreBlankAndComment(currentLoc).dropWhile(_ != ';').drop(1)
      if (tmp.isEmpty) "null" else tmp
    case _ =>
      throw new Error("illegal symbol operation")
  }
}

def dest(menemonic: String): String = {
  menemonic.filterNot(_ == ' ').toUpperCase match {
    case "NULL" => "000"
    case "M" => "001"
    case "D" => "010"
    case "MD" => "011"
    case "A" => "100"
    case "AM" => "101"
    case "AD" => "110"
    case "AMD" => "111"
  }
}

def jump(menemonic: String): String = {
  menemonic.filterNot(_ == ' ').toUpperCase match {
    case "NULL" => "000"
    case "JGT" => "001"
    case "JEQ" => "010"
    case "JGE" => "011"
    case "JLT" => "100"
    case "JNE" => "101"
    case "NLE" => "110"
    case "JMP" => "111"
  }
}

def comp(menemonic: String): String = {
  menemonic.filterNot(_ == ' ').toUpperCase match {
    case "0" => "0101010"
    case "1" => "0111111"
    case "-1" => "0111010"
    case "D" => "0001100"
    case "A" => "0110000"
    case "M" => "1110000"
    case "!D" => "0001101"
    case "!A" => "0110001"
    case "!M" => "1110001"
    case "-D" => "0001111"
    case "-A" => "0110011"
    case "-M" => "1110011"
    case "D+1" => "0011111"
    case "A+1" => "0110111"
    case "M+1" => "1110111"
    case "D-1" => "0001110"
    case "A-1" => "0110010"
    case "M-1" => "1110010"
    case "D+A" => "0000010"
    case "D+M" => "1000010"
    case "D-A" => "0010011"
    case "D-M" => "1010011"
    case "A-D" => "0000111"
    case "M-D" => "1000111"
    case "D&A" => "0000000"
    case "D&M" => "1000000"
    case "D|A" => "0010101"
    case "D|M" => "1010101"
  }
}

def symbolTransfer(s: String): String = {
  intTo15HexString(s.toInt)
}

def intTo15HexString(i: Int): String = {
  var res: String = ""
  var num: Int = i
  while(num != 0) {
    val mod = if (num % 2 == 0) "0" else "1"
    res = mod +  res
    num = num / 2
  }
  (0 until 15).map(_ => "0").drop(res.length).mkString("") + res
}

for ( i <-  0 until totalLength) {
  currentLoc = i
  commandType() match {
    case LCommand =>

    case _ =>
  }
}


val out = new java.io.PrintWriter(args(0).takeWhile(_ != '.') + "1.hack")

while (hasMoreCommands()) {
  val line = commandType() match {
    case ACommand =>
      "0" + symbolTransfer(symbol()) + "\n"
    case CCommand =>
      "111" + comp(comp()) + dest(dest())  + jump(jump()) + "\n"
    case LCommand => ""
  }
  println(ignoreBlankAndComment(currentLoc))
  print(line)
  out.print(line)
  advance()
}

out.close
