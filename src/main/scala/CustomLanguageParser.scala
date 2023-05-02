package com.github.markrenzi.dsl

import fastparse.Parsed._

import java.nio.file.{Files, Paths}
import scala.io.Source

// Abstract syntax tree (AST) data structures

/**
 * Represents the root of the AST; this is the top level
 */
sealed trait Ast

/**
 * Represents a program
 * @param statements The statements of the program as a sequence of statements
 */
case class Program(statements: Seq[Statement]) extends Ast

/**
 * Represents a statement; usually left hand assignments; this
 * is a super class
 */
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
 * @param variableReference The name of the variable
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
 * @param protocol The communication protocol of the server
 * @param functions The functions on the server
 */
case class ServerDeclaration(v: VariableReference, url: String, port: Int, protocol: String, functions: Seq[VariableReference]) extends Statement

/**
 * Represents a conditional statement
 * @param condition The condition of the conditional as an expression
 * @param body The body of the conditional as a sequence of statements
 * @param next The next conditional in the chain, if any; `None` otherwise
 * @param previousIsIf the current condition is elif; `None` otherwise
 */
case class Conditional(condition: Option[Expression], body: Seq[Statement], next: Option[Conditional], previousIsIf: Option[Boolean] = None) extends Statement

/**
 * Represents a for loop statement
 * @param ct The variable declaration of the for loop, if any; `None` otherwise
 * @param condition The condition of the for loop as an expression
 * @param redefinition The variable redefinition of the for loop, if any; `None` otherwise
 * @param body The body of the conditional as a sequence of statements
 */
case class ForLoop(ct: Option[VariableDeclaration], condition: Option[Expression], redefinition: Option[VariableDefinition], body: Seq[Statement]) extends Statement

/**
 * Represents a while loop statement
 * @param condition The condition of the while loop as an expression
 * @param body The body of the conditional as a sequence of statements
 */
case class WhileLoop(condition: Expression, body: Seq[Statement]) extends Statement

/**
 * Represents the infinite loop statement for embedded programming
 * @param body The body of the conditional as a sequence of statements
 */
case class ProgramLoop(body: Seq[Statement]) extends Statement

/**
 * Represents a Function call as a statement
 * @param func The function call as an expression
 */
case class FunctionCallAsStatement(func: FunctionCall) extends Statement

/**
 * Represents and include statement
 * @param path The function call as an expression
 */
case class Include(path: String) extends Statement

/**
 * Represents an expression; usually right hand assignments
 * and anything that evaluates to a value; this is a super class
 */
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
 * @param arrInd The index of the array, if any; -1 otherwise
 */
case class VariableReference(name: String, arrInd: Int) extends Expression

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
case class FunctionCall(variable:VariableReference, param: Seq[Expression]) extends Expression

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
  private def variableType[_: P]: P[VariableType] = P(
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
   * @return IntegerLiteral
   */
  private def integerLiteral[_: P]: P[IntegerLiteral] = P((CharIn("\\-").rep(min = 0, max = 1) ~ CharIn("0-9").rep(1)).!.map(s => IntegerLiteral(s.toInt)))

  /**
   * Parses a float literal
   * @return FloatLiteral
   */
  private def floatLiteral[_: P]: P[FloatLiteral] = P((CharIn("\\-").rep(min = 0, max = 1) ~ CharIn("0-9").rep(1) ~ "." ~ CharIn("0-9").rep(1)).!.map(s => FloatLiteral(s.toFloat)))

  /**
   * Parses a boolean literal
   * @return BooleanLiteral
   */
  private def booleanLiteral[_: P]: P[BooleanLiteral] = P(StringIn("true", "false").!.map(s => BooleanLiteral(s.toBoolean)))

  /**
   * Parses a string literal
   * @return StringLiteral
   */
  private def stringLiteral[_: P]: P[StringLiteral] = P("\"" ~ CharsWhile(_ != '\"', 0).! ~ "\"").map(StringLiteral)

  /**
   * Parses an array literal
   * @return ArrayLiteral
   */
  private def arrayLiteral[_: P]: P[ArrayLiteral] = P("{" ~ expression.rep(min = 0, sep = ",") ~ "}") .map(ArrayLiteral)

  /**
   * Parses a reference to a variable
   * @return VariableReference
   */
  private def variableReference[_: P]: P[VariableReference] = P((CharIn("a-zA-Z_") ~~ CharIn("a-zA-Z0-9_").rep).! ~~ ( "[" ~ integerLiteral ~ "]").? ).map{
    case (s, i) => if (i.isEmpty) {VariableReference(s, -1)} else {VariableReference(s, i.get.value)}
  }

  //Expression terminators
  /**
   * Parses any type of literal
   * @return Expression
   */
  private def literal[_: P]: P[Expression] = P(functionCall | floatLiteral | stringLiteral | integerLiteral | booleanLiteral | arrayLiteral | variableReference )
  private def parens[_: P]: P[Expression] = P("(" ~/ truthOr ~ ")")

  // e12
  private def factor[_: P]: P[Expression] = P(literal | parens)

  // truth comparisons
  private def truComparison[_: P]: P[String] = P(StringIn("!=", "==").!)

  // num comparisons
  private def valComparison[_: P]: P[String] = P(StringIn("<", ">", "<=", ">=").!)

  // Binary shift operations
  private def binaryShift[_: P]: P[String] = P(StringIn("<<", ">>").!)

  // Negators
  private def negator[_: P]: P[String] = P(CharIn("!~").!)

  /**
   * Parses a truth 'or' expression, e1
   * @return Operation or Expression
   */
  private def truthOr[_: P]: P[Expression] = P(truthAnd ~ ("||".! ~ truthAnd).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  /**
   * Parses a truth 'and' expression, e2
   * @return Operation or Expression
   */
  private def truthAnd[_: P]: P[Expression] = P(bitOr ~ ("&&".! ~ bitOr).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  /**
   * Parses a bitwise 'or' expression, e3
   * @return Operation or Expression
   */
  private def bitOr[_: P]: P[Expression] = P(bitXor ~ ("|".! ~ bitXor).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }

  /**
   * Parses a bitwise 'xor' expression, e4
   * @return Operation or Expression
   */
  private def bitXor[_: P]: P[Expression] = P(bitAnd ~ ("^".! ~ bitAnd).rep).map {
    case (l,r) => if(r.isEmpty) {l} else {Operation(l,r)}
  }


  /**
   * Parses a bitwise 'and' expression, e5
   * @return Operation or Expression
   */
  private def bitAnd[_: P]: P[Expression] = P(truthComparison ~ ("&".! ~ truthComparison).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }

  /**
   * Parses a truth comparison expression, e6
   * @return Operation or Expression
   */
  private def truthComparison[_: P]: P[Expression] = P(valueComparison ~ (truComparison ~ valueComparison).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }


  /**
   * Parses a value comparison expression, e7
   * @return Operation or Expression
   */
  private def valueComparison[_: P]: P[Expression] = P(shift ~ (valComparison ~ shift).rep).map {
    case (l, r) => if (r.isEmpty) {l} else {Operation(l, r)}
  }

  /**
   * Parses a binary shift operation expression, e8
   * @return Operation or Expression
   */
  private def shift[_: P]: P[Expression] = P(addSub ~ (binaryShift ~ addSub).rep).map {
    case (l, r) => if (r.isEmpty) { l } else { Operation(l, r) }
  }

  /**
   * Parses a logical negation expression, e11
   * @return Negation or Expression
   */
  private def negation[_: P]: P[Expression] = P(negator.? ~ factor).map {
    case (l, r) => if (l.isEmpty){r} else {Negation(l.getOrElse("!"), r)}
  }


  /**
   * Parses a division or multiplication expression, e10
   * @return Operation or Expression
   */
  private def divMul[_: P]: P[Expression] = P(negation ~ (CharIn("*/%").! ~/ negation).rep).map {
    case (l, r) => if (r.isEmpty){l} else {Operation(l, r)}
  }

  /**
   * Parses a addition or subtraction expression, e9
   * @return Operation or Expression
   */
  private def addSub[_: P](): P[Expression] = P(divMul ~ (CharIn("+\\-").! ~/ divMul).rep).map {
    case (l, r) => if (r.isEmpty){l} else {Operation(l, r)}
  }

  // Beginning of precedence parsing
  private def expression[_: P]: P[Expression] = P(truthOr)


  // Matching a newline
  private def newline[_: P]: P[Unit] = P((((("\r".? ~~ "\n") | "\n" ) | comment).rep(1) | End).map(_ => ()))

  /**
   * Parses comments and comsumes them
   * @return Unit
   */
  private def comment[_: P]: P[Unit] = P((("/*" ~/ (!"*/" ~ AnyChar).rep ~ "*/") ~ newline.rep(0)) | ("//" ~/ (!"\r\n" ~ !"\n" ~ AnyChar).rep) ~ newline)

  /**
   * Parses a generic statement
   * @param depth the depth of the statement indentation
   * @return Statement
   */
  private def statement[_: P](depth: Int): P[Statement] = returnStatement | includeStatement | chainDeclaration(depth) | servDef | functionDeclaration(depth) | ifConditional(depth) | variableDeclaration | variableDefinition | functionCallAsStatement | whileLoop(depth) | forLoop(depth) | programLoop(depth)

  /**
   * Parses a function call statement
   * @return FunctionCallAsStatement
   */
  private def functionCallAsStatement[_: P]: P[FunctionCallAsStatement] = (functionCall ~ newline).map(FunctionCallAsStatement)

  /**
   * Parses a function call statement
   * @return FunctionCallAsStatement
   */
  private def includeStatement[_: P]: P[Include] = ("include" ~ stringLiteral ~ newline).map {
    case StringLiteral(i) => Include(i)
  }

  /**
   * Parses a variable declaration statement
   * @return VariableDeclaration
   */
  private def variableDeclaration[_: P]: P[VariableDeclaration] =
    P(variableType ~ variableReference ~ "=" ~ expression ~ newline).map {
      case (t, VariableReference(v, -1), e) => VariableDeclaration(t, v, e)
    }

  /**
   * Parses a variable redefinition statement
   * @return VariableDefinition
   */
  private def variableDefinition[_: P]: P[VariableDefinition] =
    P(variableReference ~ "=" ~ expression ~ newline).map {
      case (v, e) => VariableDefinition(v, e)
    }

  /**
   * Parses a chain declaration statement
   * @param depth the depth of the statement indentation
   * @return ChainDeclaration
   */
  private def chainDeclaration[_: P](depth: Int): P[ChainDeclaration] =
    P("@def" ~ variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min = 0,sep = ",") ~ ")" ~ ":" ~ newline ~~ (("    " | "\t").repX(min = depth, max = depth) ~~ statement(depth + 1)).repX()).map {
      case (t,VariableReference(v, -1),s,b) => ChainDeclaration(t,v,s,b)
    }

  /**
   * Parses a function declaration statement
   * @param depth the depth of the statement indentation
   * @return FunctionDeclaration
   */
  private def functionDeclaration[_: P](depth: Int): P[FunctionDeclaration] =
    P( "def" ~ variableType ~ variableReference ~ "(" ~ (variableType ~ variableReference).rep(min=0,sep="," ) ~ ")" ~ ":" ~ newline ~~ (("    " | "\t").repX(min = depth, max = depth) ~~ statement(depth + 1)).repX()).map {
      case (t, VariableReference(v, -1), s, b) => FunctionDeclaration(t, v, s, b)
    }

  /**
   * Parses a return statement
   * @return ReturnStatement
   */
  private def returnStatement[_: P]: P[ReturnStatement] = P("return" ~/ expression.? ~ newline ).map (a => ReturnStatement(a))

  /**
   * Parses a function call expression
   * @return FunctionCall
   */
  private def functionCall[_: P]: P[FunctionCall] =
    P(variableReference ~ "(" ~ expression.rep(min = 0, sep = ",") ~ ")" ).map {
      case (v, s) => FunctionCall(v, s)
    }

  /**
   * Parses a server declaration statement
   * @return Conditional
   */
  private def servDef[_: P]: P[ServerDeclaration] =
    P("@" ~ variableReference ~ "=" ~ "(" ~ "\"" ~ CharIn("a-zA-Z0-9\\.\\-:/").rep(1).! ~ "\"" ~ "," ~ CharIn("0-9").rep(1).! ~ "," ~ variableReference ~ "," ~ "{" ~ ("@" ~~ variableReference ).rep(min = 0, sep = ",") ~ "}" ~ ")" ~ newline).map {
      case (v,u,p,VariableReference(prtcl, -1),f) => ServerDeclaration(v,u,p.toInt,prtcl,f)
    }

  /**
   * Parses an if statement
   * @param depth the depth of the statement indentation
   * @return Conditional
   */
  private def ifConditional[_: P](depth: Int): P[Conditional] =
    P("if" ~/ expression ~ ":" ~ newline ~~ (("    " | "\t").repX(min = depth, max = depth) ~~ statement(depth + 1)).repX() ~~ ("    " | "\t").repX(min = depth-1, max = depth-1) ~~ (elifConditional(depth) | elseConditional(depth)) ).map {
      case (c, b, n) => Conditional(Some(c), b, Some(n))
    }

  /**
   * Parses an elif statement
   * @param depth the depth of the statement indentation
   * @return Conditional
   */
  private def elifConditional[_: P](depth: Int): P[Conditional] =
    P("elif" ~/ expression ~ ":" ~ newline ~~ (("    " | "\t").repX(min = depth, max = depth) ~~ statement(depth + 1)).repX() ~~ ("    " | "\t").repX(min = depth-1, max = depth-1) ~~ (elifConditional(depth) | elseConditional(depth)) ).map {
      case (c, b, n) => Conditional(Some(c), b, Some(n), Some(true))
    }

  /**
   * Parses an else statement
   * @param depth the depth of the statement indentation
   * @return Conditional
   */
  private def elseConditional[_: P](depth: Int): P[Conditional] =
    P("else" ~/ ":" ~ newline ~~ (("    " | "\t").repX(min = depth, max = depth) ~~ statement(depth + 1)).repX()).map(b => Conditional(None,b,None))

  /**
   * Parses a for loop statement
   *
   * @param depth the depth of the statement indentation
   * @return ForLoop
   */
  private def forLoop[_: P](depth: Int): P[ForLoop] =
    P("for" ~/ loopVariableDeclaration.? ~ ";" ~ expression.? ~ ";" ~ loopVariableDefinition.? ~ ":" ~ newline ~~ (("    " | "\t").repX(min = depth, max = depth) ~~ statement(depth + 1)).repX()).map {
      case (t, c, d, b) => ForLoop(t, c, d, b)
    }

  private def loopVariableDeclaration[_: P]: P[VariableDeclaration] =
    P(variableType ~ variableReference ~ "=" ~ expression).map {
      case (t,VariableReference(v, -1),e) => VariableDeclaration(t,v,e)
    }

  private def loopVariableDefinition[_: P]: P[VariableDefinition] =
    P(variableReference ~ "=" ~ expression).map {
      case (v,e) => VariableDefinition(v,e)
    }

  /**
   * Parses a while loop statement
   * @param depth the depth of the statement indentation
   * @return WhileLoop
   */
  private def whileLoop[_: P](depth: Int): P[WhileLoop] =
    P("while" ~/ expression ~ ":" ~ newline ~~ (("    " | "\t").repX(min = depth, max = depth) ~~ statement(depth + 1)).repX()).map {
      case (c, b) => WhileLoop(c, b)
    }

  private def programLoop[_: P](depth: Int): P[ProgramLoop] =
    P("loop" ~/ ":" ~ newline ~~ (("    " | "\t").repX(min = depth,max = depth) ~~ statement(depth + 1)).repX()).map {
      case (b) => ProgramLoop(b)
    }

  /**
   * Parses a variable reference
   * @return
   */
  private def program[_: P]: P[Program] = P(newline.? ~ statement(1).rep ~ End).map(Program)

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
        val analyzer  = new ASTAnalyzer()
        val analysisResult = analyzer.visit(ast)
        val generator = new CodeGenerator()
        val cCode = generator.visit(ast)
        println(cCode)
        
        val outputPath = "outputC.c"
        Files.write(Paths.get(outputPath),cCode.getBytes)

      case f: Failure =>
        println(s"Parsing '$fileName' failed:")
        println(f.trace().longAggregateMsg)
    }
  }
}