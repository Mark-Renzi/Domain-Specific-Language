import fastparse._
import fastparse.Parsed._
import java.nio.file.{Files, Paths}
import scala.io.Source

// Abstract syntax tree (AST) data structures
sealed trait Ast
case class Program(statements: Seq[Statement]) extends Ast
sealed trait Statement extends Ast

/**
 * Represents a variable declaration
 * @param variableType The type of the variable
 * @param variable The name of the variable
 * @param value The value of the variable as an expression
 */
case class VariableDeclaration(variableType: VariableType, variable: String, value: Expression) extends Statement

/**
 * Represents a variable redefinition
 * @param variable The name of the variable
 * @param value The new value of the variable as an expression
 */
case class VariableDefinition(variableReference: VariableReference, value: Expression) extends Statement

/**
 * Represents a function declaration
 * @param variableType The return type of the function
 * @param variable The name of the function
 * @param param The parameters of the function as a sequence of tuples of the type and name of the parameter
 * @param body The body of the function as a sequence of statements
 */
case class FunctionDeclaration(variableType: VariableType, variable:String, param: Seq[(VariableType, VariableReference)], body: Seq[Statement]) extends Statement

/**
 * Represents a chain declaration
 * @param variableType The return type of the chain
 * @param variable The name of the chain
 * @param param The parameters of the chain as a sequence of tuples of the type and name of the parameter
 * @param body The body of the chain as a sequence of statements
 */
case class ChainDeclaration(variableType: VariableType,variable:String,param: Seq[(VariableType, VariableReference)],body: Seq[Statement]) extends Statement

/**
 * Represents a server declaration
 * @param v The name of the server
 * @param url The url of the server
 * @param port The port of the server
 * @param functions The functions on the server
 */
case class ServerDeclaration(v: VariableReference, url: String, port: Int, functions: Seq[VariableReference]) extends Statement

/**
 * Represents a conditional statement
 * @param condition The condition of the conditional as an expression
 * @param body The body of the conditional as a sequence of statements
 * @param next The next conditional in the chain as a sequence of conditionals
 */
case class Conditional(condition: Option[Expression], body: Seq[Statement], next: Seq[Conditional]) extends Statement

/**
 * Represents a Function call as a statement
 * @param func The function call as an expression
 */
case class FunctionCallAsStatement(func: FunctionCall) extends Statement

sealed trait Expression extends Ast

/**
 * Represents an integer literal
 * @param value The value of the literal
 */
case class IntegerLiteral(value: Int) extends Expression

/**
 * Represents a float literal
 * @param value The value of the literal
 */
case class FloatLiteral(value: Float) extends Expression

/**
 * Represents a boolean literal
 * @param value The value of the literal
 */
case class BooleanLiteral(value: Boolean) extends Expression

/**
 * Represents a string literal
 * @param value The value of the literal
 */
case class StringLiteral(value: String) extends Expression

/**
 * Represents an array literal
 * @param v The values of the array as a sequence of expressions
 */
case class ArrayLiteral( v: Seq[Expression]) extends Expression

/**
 * Represents a variable reference
 * @param name The name of the variable
 */
case class VariableReference(name: String) extends Expression

/**
 * Represents an operation
 * @param l The left hand side of the operation as an expression
 * @param r The right hand side of the operation as a sequence of tuples of the operator and the expression
 */
case class Operation(l: Expression, r: Seq[(String, Expression)]) extends Expression

/**
 * Represents a negation
 * @param l The left hand side of the negation, "!"
 * @param r The right hand side of the negation as an expression
 */
case class Negation(l: String, r: Expression) extends Expression

/**
 * Represents a function call
 * @param variable The name of the function
 * @param param The parameters of the function as a sequence of expressions
 */
case class FunctionCall(variable:VariableReference, param: Seq[(Expression)]) extends Expression

/**
 * Represents a chain call
 * @param v The value of the return, if any; `None` otherwise
 */
case class ReturnStatement(v: Option[Expression]) extends Statement

/**
 * Represents variable type
 * @param t The type of the variable as a string
 * @param arr The number of array dimensions, -1 if not an array
 */
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
   *
   * @tparam _
   * @return IntegerLiteral
   */
  def integerLiteral[_: P]: P[IntegerLiteral] = P((CharIn("\\-").rep(min = 0, max = 1) ~ CharIn("0-9").rep(1)).!.map(s => IntegerLiteral(s.toInt)))

  /**
   * Parses a float literal
   *
   * @tparam _
   * @return FloatLiteral
   */
  def floatLiteral[_: P]: P[FloatLiteral] = P((CharIn("\\-").rep(min = 0, max = 1) ~ CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).!.map(s => FloatLiteral(s.toFloat)))

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
  def variableReference[_: P]: P[VariableReference] = P((CharIn("a-zA-Z_") ~~ CharIn("a-zA-Z0-9_").rep).!.map(VariableReference))

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
  def statement[_: P]: P[Statement] = (returnStatement | chainDeclaration | servDef | functionDeclaration | ifConditional | variableDeclaration | variableDefinition | functionCallAsStatement)

  /**
   * Parses a function call statement
   * @tparam _
   * @return FunctionCallAsStatement
   */
  def functionCallAsStatement[_: P]: P[FunctionCallAsStatement] = (functionCall ~ newline).map(FunctionCallAsStatement)

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
   * Parses a chain declaration statement
   * @tparam _
   * @return ChainDeclaration
   */
  def chainDeclaration[_: P]: P[ChainDeclaration] =
    P("@def" ~ variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min = 0,sep = ",") ~ ")" ~ ":" ~ newline ~~ ((" ".repX(min = 1,max = 4) | "\t") ~~ statement).repX()).map {
      case (t,VariableReference(v),s,b) => ChainDeclaration(t,v,s,b)
    }

  /**
   * Parses a function declaration statement
   * @tparam _
   * @return FunctionDeclaration
   */
  def functionDeclaration[_: P]: P[FunctionDeclaration] =
    P( "def" ~ variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min=0,sep="," ) ~ ")" ~ ":" ~ newline ~~ ((" ".repX(min=1, max=4) | "\t") ~~ statement).repX()).map {
      case (t, VariableReference(v), s, b) => FunctionDeclaration(t, v, s, b)
    }

  /**
   * Parses a return statement
   * @tparam _
   * @return ReturnStatement
   */
  def returnStatement[_: P]: P[ReturnStatement] = P("return" ~/ expression.? ~ newline ).map{
    case a => ReturnStatement(a)
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
   * Parses a server declaration statement
   * @tparam _
   * @return Conditional
   */
  def servDef[_: P]: P[ServerDeclaration] =
    P("@" ~~ variableReference ~ "=" ~ "(" ~ CharIn("a-zA-Z0-9.\\-:/").rep(1).! ~ "," ~ CharIn("0-9").rep(1).! ~ "," ~ "{" ~ ("@" ~~ variableReference ).rep(min = 0, sep = ",") ~ "}" ~ ")").map {
      case (v,u,p,f) => ServerDeclaration(v,u,p.toInt,f)
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
    val fileName = "CustomLanguage.txt" //name of the file to be parsed from string to AST
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