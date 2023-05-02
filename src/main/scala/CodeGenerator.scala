package com.github.markrenzi.dsl

trait ASTVisitor {
  def visit(node: Expression): String
  def visit(node: Statement): String
  def visit(node: Program): String
  def visit(node: VariableType): String
}





class CodeGenerator extends ASTVisitor {
  def visit(node: Expression): String = node match {
    case lit: BooleanLiteral => if (lit.value) "1" else "0"
    case lit: StringLiteral => "\"" + lit.value + "\""
    case arr: ArrayLiteral => "{" + arr.v.map(visit).mkString(", ") + "}"
    case vr: VariableReference =>
      val arrayIndex = if (vr.arrInd != -1) s"[${vr.arrInd}]" else ""
      vr.name + arrayIndex
    case op: Operation =>
      val left = visit(op.l)
      val right = op.r.map { case (oper, expr) => oper + " " + visit(expr) }.mkString(" ")
      s"$left $right"
    case num: IntegerLiteral => num.value.toString
    case flt: FloatLiteral => flt.value.toString
    case neg: Negation => s"${neg.l}${visit(neg.r)}"
    case fc: FunctionCall =>
      val params = fc.param.map(visit).mkString(", ")
      s"${visit(fc.variable)}($params)"
    case _ => throw new RuntimeException(s"Unsupported node type: $node")
  }


  def visit(node: Statement): String = {
    node match {
      case retStmt: ReturnStatement =>
        retStmt.v.map(expr => s"return ${visit(expr)};").getOrElse("return;")
      case varDecl: VariableDeclaration =>
        s"${visit(varDecl.variableType)} ${varDecl.variable} = ${visit(varDecl.value)};"
      case varDef: VariableDefinition =>
        s"${visit(varDef.variableReference)} = ${visit(varDef.value)};"
      case funcDecl: FunctionDeclaration =>
        val params = funcDecl.param.map { case (varType,varRef) => s"${visit(varType)} ${visit(varRef)}" }.mkString(", ")
        val body = funcDecl.body.map(visit).mkString("\n")
        s"${visit(funcDecl.variableType)} ${funcDecl.variable}($params) {\n$body\n}"
      // Add other cases for different Statement types
      case cond: Conditional =>
        val prefix = if(cond.previousIsIf.getOrElse(false)) "else " else ""
        val condition = cond.condition.map(c => s"${prefix}if (${visit(c)})").getOrElse("else")
        val body = cond.body.map(visit).mkString("\n")
        val next = cond.next.map(visit).getOrElse("")
        s"$condition {\n$body\n} $next"
      case forLoop: ForLoop =>
        val init = forLoop.ct.map(visit).getOrElse("").stripSuffix(";")
        val condition = forLoop.condition.map(visit).getOrElse("")
        val increment = forLoop.redefinition.map(visit).getOrElse("").stripSuffix(";")
        val body = forLoop.body.map(visit).mkString("\n")
        s"for ($init; $condition; $increment) {\n$body\n}"
      case whileLoop: WhileLoop =>
        val condition = visit(whileLoop.condition)
        val body = whileLoop.body.map(visit).mkString("\n")
        s"while ($condition) {\n$body\n}"
      case programLoop: ProgramLoop =>
        val body = programLoop.body.map(visit).mkString("\n")
        s"while (1) {\n$body\n}"
      case funcCallStmt: FunctionCallAsStatement =>
        s"${visit(funcCallStmt.func)};"
      case includeStmt: Include =>
        s"#include \"${includeStmt.path}\""
      case _ => ""
    }
  }

  override def visit(node: VariableType): String = {
    val baseType = node.t match {
      case "bool" => "int"
      case "string" => "char *"
      case "u8" => "uint8_t"
      case "u16" => "uint16_t"
      case "u32" => "uint32_t"
      case "u64" => "uint64_t"
      case "i8" => "int8_t"
      case "i16" => "int16_t"
      case "i32" => "int32_t"
      case "i64" => "int64_t"
      case "f32" => "float"
      case "f64" => "double"
      case "void" => "void"
      case _ => throw new RuntimeException(s"Unsupported type: ${node.t}")
    }

    val arrayDimensions = if (node.arr != -1) "[]" * node.arr else ""
    s"$baseType$arrayDimensions"
  }


  def visit(node: Program): String = {
    node.statements.map(visit).mkString("\n")
  }
}
