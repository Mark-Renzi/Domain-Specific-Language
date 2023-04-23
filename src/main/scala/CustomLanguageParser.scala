import fastparse._
import fastparse.Parsed._
import java.nio.file.{Files, Paths}
import scala.io.Source

// AST data structures
sealed trait Ast
case class Program(statements: Seq[Statement]) extends Ast
sealed trait Statement extends Ast

case class VariableDeclaration(variableType: VariableType, variable: String, value: Expression) extends Statement
case class VariableDefinition(variable: Expression, value: Expression) extends Statement
case class FunctionDeclaration(variableType: VariableType, variable:String, param: Seq[(VariableType, VariableReference)], body: Seq[Statement], ret: Option[Option[Expression]]) extends Statement
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

  /**
   * Parses an integer literal
   * @tparam _
   * @return IntegerLiteral
   */
  def integerLiteral[_: P]: P[IntegerLiteral] = P(CharIn("0-9").rep(1).!.map(s => IntegerLiteral(s.toInt)))

  /**
   * Parses a float literal
   * @tparam _
   * @return FloatLiteral
   */
  def floatLiteral[_: P]: P[FloatLiteral] = P((CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).!.map(s => FloatLiteral(s.toFloat)))

  /**
   * Parses a boolean literal
   * @tparam _
   * @return BooleanLiteral
   */
  def booleanLiteral[_: P]: P[BooleanLiteral] = P(StringIn("true", "false").!.map(s => BooleanLiteral(s.toBoolean)))

  /**
   * Parses a string literal
   * @tparam _
   * @return StringLiteral
   */
  def stringLiteral[_: P]: P[StringLiteral] = P("\"" ~ CharsWhile(_ != '\"', 0).! ~ "\"").map(StringLiteral)

  /**
   * Parses an array literal
   * @tparam _
   * @return ArrayLiteral
   */
  def arrayLiteral[_: P]: P[ArrayLiteral] = P("{" ~ (expression).rep(min = 0, sep = ",") ~ "}") .map(ArrayLiteral)

  /**
   * Parses a reference to a variable
   * @tparam _
   * @return VariableReference
   */
  def variableReference[_: P]: P[VariableReference] = P((CharIn("a-zA-Z") ~~ CharIn("a-zA-Z0-9").rep).!.map(VariableReference))

  //Expression terminators
  /**
   * Parses any type of literal
   * @tparam _
   * @return Expression
   */
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

  /**
   * Parses a truth 'or' expression, e1
   * @tparam _
   * @return Operation or Expression
   */
  def truthOr[_: P]: P[Expression] = P(truthAnd ~ ("||".! ~ truthAnd).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  /**
   * Parses a truth 'and' expression, e2
   * @tparam _
   * @return Operation or Expression
   */
  def truthAnd[_: P]: P[Expression] = P(bitOr ~ ("&&".! ~ bitOr).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  /**
   * Parses a bitwise 'or' expression, e3
   * @tparam _
   * @return Operation or Expression
   */
  def bitOr[_: P]: P[Expression] = P(bitXor ~ ("|".! ~ bitXor).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  /**
   * Parses a bitwise 'xor' expression, e4
   * @tparam _
   * @return Operation or Expression
   */
  def bitXor[_: P]: P[Expression] = P(bitAnd ~ ("^".! ~ bitAnd).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }


  /**
   * Parses a bitwise 'and' expression, e5
   * @tparam _
   * @return Operation or Expression
   */
  def bitAnd[_: P]: P[Expression] = P(truthComparison ~ ("&".! ~ truthComparison).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }

  /**
   * Parses a truth comparison expression, e6
   * @tparam _
   * @return Operation or Expression
   */
  def truthComparison[_: P]: P[Expression] = P(valueComparison ~ (truComparison ~ valueComparison).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }


  /**
   * Parses a value comparison expression, e7
   * @tparam _
   * @return Operation or Expression
   */
  def valueComparison[_: P]: P[Expression] = P(shift ~ (valComparison ~ shift).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }

  /**
   * Parses a binary shift operation expression, e8
   * @tparam _
   * @return Operation or Expression
   */
  def shift[_: P]: P[Expression] = P(addSub ~ (binaryShift ~ addSub).rep).map {
    case (l, r) => if (r.isEmpty) { l } else { Operation(l, r) }
  }


  /**
   * Parses a logical negation expression, e11
   * @tparam _
   * @return Negation or Expression
   */
  def negation[_: P]: P[Expression] = P(negator.? ~ factor).map {
    case (l, r) => if (l.isEmpty){r} else {Negation(l.getOrElse("!"), r)}
  }


  /**
   * Parses a division or multiplication expression, e10
   * @tparam _
   * @return Operation or Expression
   */
  def divMul[_: P]: P[Expression] = P(negation ~ (CharIn("*/%").! ~/ negation).rep).map {
    case (l, r) => if (r.isEmpty){l} else {Operation(l, r)}
  }

  /**
   * Parses a addition or subtraction expression, e9
   * @tparam _
   * @return Operation or Expression
   */
  def addSub[_: P]: P[Expression] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
    case (l, r) => if (r.isEmpty){l} else {Operation(l, r)}
  }

  // Beginning of precedence parsing
  def expression[_: P]: P[Expression] = P(truthOr)


  // Matching a newline
  def newline[_: P]: P[Unit] = P((("\r".? ~ "\n" | "\r") | End).map(_ => ()))

  /**
   * Parses a generic statement
   * @tparam _
   * @return Statement
   */
  def statement[_: P]: P[Statement] = (functionDeclaration | ifConditional | variableDeclaration | variableDefinition)

  /**
   * Parses a variable declaration statement
   * @tparam _
   * @return VariableDeclaration
   */
  def variableDeclaration[_: P]: P[VariableDeclaration] =
    P(variableType ~ variableReference ~ "=" ~ expression ~ newline).map {
      case (t, VariableReference(v), e) => VariableDeclaration(t, v, e)
    }

  /**
   * Parses a variable redefinition statement
   * @tparam _
   * @return VariableDefinition
   */
  def variableDefinition[_: P]: P[VariableDefinition] =
    P(variableReference ~ "=" ~ expression ~ newline).map {
      case (v, e) => VariableDefinition(v, e)
    }

  /**
   * Parses a function declaration statement
   * @tparam _
   * @return FunctionDeclaration
   */
  def functionDeclaration[_: P]: P[FunctionDeclaration] =
    P(variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min=0,sep="," ) ~ ")" ~ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX() ~~/ (((" " | "\t").repX(1) ~~ "return") ~/ (expression).? ~ newline).?).map {
      case (t, VariableReference(v), s, b, r) => FunctionDeclaration(t, v, s, b, r)
    }

  /**
   * Parses a function call expression
   * @tparam _
   * @return FunctionCall
   */
  def functionCall[_: P]: P[FunctionCall] =
    P(variableReference ~ "(" ~ (expression).rep(min = 0, sep = ",") ~ ")" ).map {
      case (v, s) => FunctionCall(v, s)
    }

  /**
   * Parses an if statement
   * @tparam _
   * @return Conditional
   */
  def ifConditional[_: P]: P[Conditional] =
    P("if" ~/ expression ~ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX() ~~ (elifConditional | elseConditional).repX() ).map {
      case (c, b, n) => Conditional(Some(c), b, n)
    }

  /**
   * Parses an elif statement
   * @tparam _
   * @return Conditional
   */
  def elifConditional[_: P]: P[Conditional] =
    P("elif" ~/ expression ~ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX() ~~ (elifConditional | elseConditional).repX() ).map {
      case (c, b, n) => Conditional(Some(c), b, n)
    }

  /**
   * Parses an else statement
   * @tparam _
   * @return Conditional
   */
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
