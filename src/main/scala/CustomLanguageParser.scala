import fastparse._
import fastparse.Parsed._
import java.nio.file.{Files, Paths}
import scala.io.Source

// Define the AST data structures
sealed trait Ast
case class Program(statements: Seq[Statement]) extends Ast
sealed trait Statement extends Ast
case class VariableDeclaration(variableType: String, variable: String, value: Expression) extends Statement
case class VariableDefinition(variable: String, value: Expression) extends Statement
sealed trait Expression extends Ast
case class IntegerLiteral(value: Int) extends Expression
case class FloatLiteral(value: Float) extends Expression
case class BooleanLiteral(value: Boolean) extends Expression
case class StringLiteral(value: String) extends Expression
case class VariableReference(name: String) extends Expression
case class FunctionDeclaration(variableType: String, variable:String, param: Seq[(String, VariableReference)]) extends Statement


// Define the parser
object CustomLanguageParser {
  import fastparse._, MultiLineWhitespace._

  // Parse a variable type
  def variableType[_: P]: P[String] = P(
    StringIn(
      "bool", "string",
      "u8", "u16", "u32", "u64",
      "i8", "i16", "i32", "i64",
      "f32", "f64",
      "void"
    ).!
  )

  // Parse an integer literal
  def integerLiteral[_: P]: P[IntegerLiteral] = P(CharIn("0-9").rep(1).!.map(s => IntegerLiteral(s.toInt)))

  // Parse a float literal
  def floatLiteral[_: P]: P[FloatLiteral] = P((CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).!.map(s => FloatLiteral(s.toFloat)))

  // Ask prof if something like this is possible
//  def numLiteral[_: P] = P(((CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)) | (CharIn("0-9").rep(1).!.map(s => IntegerLiteral(s.toInt)))).!.map(
//    case (s,a) => FloatLiteral(s.toFloat)
//    case _ => IntegerLiteral(s.toInt)
//  ))

  // Parse a boolean literal
  def booleanLiteral[_: P]: P[BooleanLiteral] = P(StringIn("true", "false").!.map(s => BooleanLiteral(s.toBoolean)))

  // Parse a string literal
  def stringLiteral[_: P]: P[StringLiteral] = P("\"" ~/ CharsWhile(_ != '\"', 0).! ~ "\"").map(StringLiteral)

  // Parse a variable reference
  def variableReference[_: P]: P[VariableReference] = P(CharIn("a-zA-Z").rep(1).!.map(VariableReference))

  // Parse an expression (either an integer literal, float literal, boolean literal, string literal, or a variable reference)
  def expression[_: P]: P[Expression] = P((floatLiteral | integerLiteral | booleanLiteral | stringLiteral | variableReference))

  // Parse a variable declaration statement
  def variableDeclaration[_: P]: P[VariableDeclaration] =
    P(variableType ~ variableReference ~ "=" ~ expression).map {
      case (t, VariableReference(v), e) => VariableDeclaration(t, v, e)
    }

  // Parse a variable redefinition
  def variableDefinition[_: P]: P[VariableDefinition] =
    P(variableReference ~ "=" ~ expression).map {
      case (VariableReference(v), e) => VariableDefinition(v, e)
    }
  
  // Parse a function declaration statement
  def functionDeclaration[_: P]: P[FunctionDeclaration] =
    P(variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min=0,sep="," ) ~ ")" ~ ":").map {
      case (t, VariableReference(v), s) => FunctionDeclaration(t, v, s)
    }
  
  // Parse a program
  def program[_: P]: P[Program] = P((variableDeclaration | variableDefinition | functionDeclaration).rep ~ End).map(Program)

  // Parse a file
  def parseFile(filename: String): Parsed[Program] = {
    val source = Source.fromFile(filename)
    val content = try source.mkString finally source.close()
    parse(content, program(_))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = "CustomLanguage.txt"
    val result = CustomLanguageParser.parseFile(fileName)

    result match {
      case Success(ast, _) =>
        println(s"Parsing '$fileName' succeeded:")
        println(ast)
      case f: Failure =>
        println(s"Parsing '$fileName' failed:")
        println(f.trace().longAggregateMsg)
    }
  }
}
