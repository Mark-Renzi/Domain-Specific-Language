import fastparse._
import fastparse.Parsed._
import java.nio.file.{Files, Paths}
import scala.io.Source

// Define the AST data structures
sealed trait Ast
case class Program(statements: Seq[Statement]) extends Ast
sealed trait Statement extends Ast
case class VariableDeclaration(variableType: VariableType, variable: String, value: Expression) extends Statement
case class VariableDefinition(variable: Expression, value: Expression) extends Statement
case class FunctionDeclaration(variableType: VariableType, variable:String, param: Seq[(VariableType, VariableReference)], body: Seq[Statement]) extends Statement
case class ChainDeclaration(variableType: VariableType,variable:String,param: Seq[(VariableType, VariableReference)],body: Seq[Statement]) extends Statement
case class ServerDeclaration(v: VariableReference, url: String, port: Int, functions: Seq[VariableReference]) extends Statement
case class Conditional(condition: Option[Expression], body: Seq[Statement], next: Seq[Conditional]) extends Statement
sealed trait Expression extends Ast
case class IntegerLiteral(value: Int) extends Expression
case class FloatLiteral(value: Float) extends Expression
case class BooleanLiteral(value: Boolean) extends Expression
case class StringLiteral(value: String) extends Expression
case class ArrayLiteral( v: Seq[Expression]) extends Expression
case class VariableReference(name: String) extends Expression
case class Operation(l: Expression, r: Seq[(String, Expression)]) extends Expression
case class Negation(l: String, r: Expression) extends Expression
case class FunctionCall(variable:VariableReference, param: Seq[(Expression)]) extends Expression
case class ReturnStatement(v: Option[Expression]) extends Statement
case class VariableType(t: String, arr: Integer)


// Define the parser
object CustomLanguageParser {
  import fastparse._, SingleLineWhitespace._

  // Parse a variable type
  def variableType[_: P]: P[VariableType] = P(
    StringIn(
      "bool", "string",
      "u8", "u16", "u32", "u64",
      "i8", "i16", "i32", "i64",
      "f32", "f64",
      "void"
    ).! ~~ (("[" ~ CharIn("0-9").rep.! ~ "]") | ("[".! ~ "]") ).?
  ) .map{
    case (t, a) => if (a.isEmpty) {VariableType(t, -1)} else {VariableType(t, if (a.getOrElse("err").equals("")) {0} else {a.getOrElse("0").toInt} )}
  }

  // Parse an integer literal
  def integerLiteral[_: P]: P[IntegerLiteral] = P((CharIn("\\-").rep(min = 0, max = 1) ~ CharIn("0-9").rep(1)).!.map(s => IntegerLiteral(s.toInt)))

  // Parse a float literal
  def floatLiteral[_: P]: P[FloatLiteral] = P((CharIn("\\-").rep(min = 0, max = 1) ~ CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).!.map(s => FloatLiteral(s.toFloat)))

  // Parse a boolean literal
  def booleanLiteral[_: P]: P[BooleanLiteral] = P(StringIn("true", "false").!.map(s => BooleanLiteral(s.toBoolean)))

  // Parse a string literal
  def stringLiteral[_: P]: P[StringLiteral] = P("\"" ~ CharsWhile(_ != '\"', 0).! ~ "\"").map(StringLiteral)

  // parse an array literal
  def arrayLiteral[_: P]: P[ArrayLiteral] = P("{" ~ (expression).rep(min = 0, sep = ",") ~ "}") .map(ArrayLiteral)

  // Parse a variable reference
  def variableReference[_: P]: P[VariableReference] = P((CharIn("a-zA-Z_") ~~ CharIn("a-zA-Z0-9_").rep).!.map(VariableReference))

  // expression terminators
  def literal[_: P]: P[Expression] = P(functionCall | floatLiteral | stringLiteral | integerLiteral | booleanLiteral | arrayLiteral | variableReference )
  def parens[_: P]: P[Expression] = P("(" ~/ truthOr ~ ")")

  // e12
  def factor[_: P]: P[Expression] = P(literal | parens)

  // truth comparisons
  def truComparison[_: P]: P[String] = P(StringIn("!=", "==").!)

  // num comparisons
  def valComparison[_: P]: P[String] = P(StringIn("<", ">", "<=", ">=").!)

  // Binary shift operations
  def binaryShift[_: P]: P[String] = P(StringIn("<<", ">>").!)

  // Negators
  def negator[_: P]: P[String] = P(CharIn("!~").!)

  // e1
  def truthOr[_: P]: P[Expression] = P(truthAnd ~ ("||".! ~ truthAnd).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  // e2
  def truthAnd[_: P]: P[Expression] = P(bitOr ~ ("&&".! ~ bitOr).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  // e3
  def bitOr[_: P]: P[Expression] = P(bitXor ~ ("|".! ~ bitXor).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  // e4
  def bitXor[_: P]: P[Expression] = P(bitAnd ~ ("^".! ~ bitAnd).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }


  // e5
  def bitAnd[_: P]: P[Expression] = P(truthComparison ~ ("&".! ~ truthComparison).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }

  // e6
  def truthComparison[_: P]: P[Expression] = P(valueComparison ~ (truComparison ~ valueComparison).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }


  // e7
  def valueComparison[_: P]: P[Expression] = P(shift ~ (valComparison ~ shift).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }

  // e8
  def shift[_: P]: P[Expression] = P(addSub ~ (binaryShift ~ addSub).rep).map {
    case (l, r) => if (r.isEmpty) { l } else { Operation(l, r) }
  }

  // e11
  def negation[_: P]: P[Expression] = P(negator.? ~ factor).map {
    case (l, r) => if (l.isEmpty){r} else {Negation(l.getOrElse("!"), r)}
  }


  // e10
  def divMul[_: P]: P[Expression] = P(negation ~ (CharIn("*/%").! ~/ negation).rep).map {
    case (l, r) => if (r.isEmpty){l} else {Operation(l, r)}
  }

  // e9
  def addSub[_: P]: P[Expression] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
    case (l, r) => if (r.isEmpty){l} else {Operation(l, r)}
  }

  def expression[_: P]: P[Expression] = P(truthOr)


  // Matching a newline
  def newline[_: P]: P[Unit] = P((("\r".? ~ "\n" | "\r") | End).map(_ => ()))

  // Parse a generic statement
  def statement[_: P]: P[Statement] = (returnStatement | chainDeclaration | servDef | functionDeclaration | ifConditional | variableDeclaration | variableDefinition)

  // Parse a variable declaration statement
  def variableDeclaration[_: P]: P[VariableDeclaration] =
    P(variableType ~ variableReference ~ "=" ~ expression ~ newline).map {
      case (t, VariableReference(v), e) => VariableDeclaration(t, v, e)
    }

  // Parse a variable redefinition
  def variableDefinition[_: P]: P[VariableDefinition] =
    P(variableReference ~ "=" ~ expression ~ newline).map {
      case (v, e) => VariableDefinition(v, e)
    }

  // Parse a server declaration statement
  def chainDeclaration[_: P]: P[ChainDeclaration] =
    P("@def" ~ variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min = 0,sep = ",") ~ ")" ~ ":" ~ newline ~~ ((" ".repX(min = 1,max = 4) | "\t") ~~ statement).repX()).map {
      case (t,VariableReference(v),s,b) => ChainDeclaration(t,v,s,b)
    }

  // Parse a function declaration statement
  def functionDeclaration[_: P]: P[FunctionDeclaration] =
    P( "def" ~ variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min=0,sep="," ) ~ ")" ~ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX()).map {
      case (t, VariableReference(v), s, b) => FunctionDeclaration(t, v, s, b)
    }

  // parse a return statement
  def returnStatement[_: P]: P[ReturnStatement] = P("return" ~/ expression.? ~ newline ).map{
    case a => ReturnStatement(a)
  }

  // Parse a function call
  def functionCall[_: P]: P[FunctionCall] =
    P(variableReference ~ "(" ~ (expression).rep(min = 0, sep = ",") ~ ")" ).map {
      case (v, s) => FunctionCall(v, s)
    }

  // Parse a server declaration
  def servDef[_: P]: P[ServerDeclaration] =
    P("@" ~~ variableReference ~ "=" ~ "(" ~ CharIn("a-zA-Z0-9.\\-:/").rep(1).! ~ "," ~ CharIn("0-9").rep(1).! ~ "," ~ "{" ~ ("@" ~~ variableReference ).rep(min = 0, sep = ",") ~ "}" ~ ")").map {
      case (v,u,p,f) => ServerDeclaration(v,u,p.toInt,f)
    }

  // Parse an if statement
  def ifConditional[_: P]: P[Conditional] =
    P("if" ~/ expression ~ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX() ~~ (elifConditional | elseConditional).repX() ).map {
      case (c, b, n) => Conditional(Some(c), b, n)
    }

  // Parse an elif statement
  def elifConditional[_: P]: P[Conditional] =
    P("elif" ~/ expression ~ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX() ~~ (elifConditional | elseConditional).repX() ).map {
      case (c, b, n) => Conditional(Some(c), b, n)
    }

  // Parse an else statment
  def elseConditional[_: P]: P[Conditional] =
    P("else" ~/ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX()).map {
      case (b) => Conditional(None, b, Seq[Conditional]())
    }
  
  // Parse a program
  def program[_: P]: P[Program] = P(statement.rep ~ End).map(Program)

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