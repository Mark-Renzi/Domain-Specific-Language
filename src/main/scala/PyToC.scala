import fastparse._
import NoWhitespace._

object PyToC {
  def parseVariableDef[_: P]: P[(String, String)] = P("var " ~ CharIn("a-z").! ~ "=" ~ (CharPred(_ != '\"') | CharIn(" ")).rep.!).map { case (name, value) => (name, value) }

  def parseFile(filename: String): String = {
    val content = io.Source.fromFile(filename).mkString
    parse(content, parseVariableDef(_)) match {
      case Parsed.Success((name, value), _) => s"int $name = $value;"
      case Parsed.Failure(_, _, _) => "Failed to parse file."
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.length == 1 && args(0).endsWith(".py")) {
      val inputFilename = args(0)
      val outputFilename = inputFilename.replace(".py", ".c")
      val outputFileContent = parseFile(inputFilename)

      val writer = new java.io.PrintWriter(new java.io.File(outputFilename))
      writer.write(outputFileContent)
      writer.close()

      println(s"Output file created: $outputFilename")
    } else {
      println("Usage: PyToC <filename.py>")
    }
  }
}
