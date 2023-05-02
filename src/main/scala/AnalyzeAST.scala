package com.github.markrenzi.dsl

trait TypeVisitor {
  def visit(node: Expression): VariableType
  def visit(node: Statement): VariableType
  def visit(node: Program): VariableType
  def visit(node: VariableType): String
}

class Environment{ //TODO NEW SCOPE STUFF
    var parent = Environment()
    var isEmpty = true
    private var E:Map[String, VariableType] = Map()

    def addVar(id: String, vartype: VariableType): Unit = 
        E += (id -> vartype)
        isEmpty = false
    
    def checkType(id: String): VariableType = 
        if( E.contains(id)) {
            return E(id)
        } else if(parent.checkType(id)!=VariableType("void", -1)){
            return parent.checkType(id)
        }
        else {
            return VariableType("void", -1)
        }
    
    def setParent(e: Environment): Unit = 
        parent = e



}
class FunctionSignature(var returnType: VariableType, params = Seq[(VariableType, VariableReference)]){

}

class FunctionEnvironment{
    private var F:Map[String, FunctionSignature] = Map()

    def addFunc(id: String, signature: FunctionSignature): Unit = 
        F += (id -> signature)
    
    def checkType(id: String): VariableType = 
        if( F.contains(id)) {
            return F(id).returnType
        } else {
            return VariableType("void", 0)
        }

    def checkParams(id: String): Seq[(VariableType, VariableReference)] = 
        if(F.contains(id)) {
            return F(id).params
        } else {
            return Seq[(VariableType, VariableReference)]()
        }

}


class ASTAnalyzer extends TypeVisitor {
    var E = new Environment()
    var F = new FunctionEnvironment()

  def matches(t1: VariableType, t2: VariableType, op: String): Boolean = {
    val equalTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64", "bool", "string")
    val arithmeticTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64")
    val shiftTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64")
    val logicalTypes:List[String] = List("bool")
    val stringTypes:List[String] = List("string")
    val floatTypes:List[String] = List("f32", "f64")
    
    op match {
        case "+" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t) || stringTypes.contains(t1.t) && stringTypes.contains(t2.t)
        case "-" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case "*" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case "/" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case "%" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case "<<" => 
            shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)
        case ">>" => 
            shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)
        case ">" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case "<" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case ">=" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case "<=" => 
            arithmeticTypes.contains(t1.t) && arithmeticTypes.contains(t2.t)
        case "==" => 
            equalTypes.contains(t1.t) && equalTypes.contains(t2.t)
        case "!=" => 
            equalTypes.contains(t1.t) && equalTypes.contains(t2.t)
        case "&" => 
            shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)
        case "^" => 
            shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)
        case "|" => 
            shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)
        case "~" => 
            shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)
        case "&&" => 
            logicalTypes.contains(t1.t) && logicalTypes.contains(t2.t)
        case "||" => 
            logicalTypes.contains(t1.t) && logicalTypes.contains(t2.t)
        case "=" =>
            (shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)) || (logicalTypes.contains(t1.t) && logicalTypes.contains(t2.t)) || (floatTypes.contains(t1.t) && floatTypes.contains(t2.t)) || (stringTypes.contains(t1.t) && stringTypes.contains(t2.t))
        case _ => throw new RuntimeException(s"bad thing happened, $op")
    }
  }

  def resultType(t1: VariableType, t2: VariableType, op: String): VariableType = {
    val signedTypes:List[String] = List("i8", "i16", "i32", "i64")
    val floatTypes:List[String] = List("f32", "f64")
    val arithmeticTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64")
    val shiftTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64")
    val stringTypes:List[String] = List("string")
    val logicalTypes:List[String] = List("bool")
    op match {
        case "+" => 
            if(floatTypes.contains(t1.t) || floatTypes.contains(t2.t)){
                VariableType("f64",-1)
            } else if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            } else if(stringTypes.contains(t1.t) && stringTypes.contains(t2.t)){
                VariableType("string", -1)
            } else{
                VariableType("u64",-1)
            }
            
        case "-" => 
            if(floatTypes.contains(t1.t) || floatTypes.contains(t2.t)){
                VariableType("f64",-1)
            } else if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            } else{
                VariableType("u64",-1)
            }
            
        case "*" => 
            if(floatTypes.contains(t1.t) || floatTypes.contains(t2.t)){
                VariableType("f64",-1)
            } else if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            }
            VariableType("u64",-1)
        case "/" => 
            if(floatTypes.contains(t1.t) || floatTypes.contains(t2.t)){
                VariableType("f64",-1)
            } else if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            }
            VariableType("u64",-1)
        case "%" => 
            if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            }
            VariableType("u64",-1)
        case "<<" => 
            VariableType("u64",-1)
        case ">>" => 
            VariableType("u64",-1)
        case ">" => 
            VariableType("bool",-1)
        case "<" => 
            VariableType("bool",-1)
        case ">=" => 
            VariableType("bool",-1)
        case "<=" => 
            VariableType("bool",-1)
        case "==" => 
            VariableType("bool",-1)
        case "!=" => 
            VariableType("bool",-1)
        case "&" => 
            if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            }
            VariableType("u64",-1)
        case "^" => 
            if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            }
            VariableType("u64",-1)
        case "|" => 
            if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            }
            VariableType("u64",-1)
        case "~" => 
            if(signedTypes.contains(t1.t) || signedTypes.contains(t2.t)){
                VariableType("i64",-1)
            }
            VariableType("u64",-1)
        case "&&" => 
            VariableType("bool",-1)
        case "||" => 
            VariableType("bool",-1)
        case _ => throw new RuntimeException(s"bad thing happened")
    }
  }
  
  def visit(node: Expression): VariableType = node match {
    case lit: BooleanLiteral => VariableType("bool", -1)
    case lit: StringLiteral => VariableType("string", -1)
    case arr: ArrayLiteral => 
        val types = arr.v.map(visit)
        println(types)
        VariableType("void",-1) //
    case vr: VariableReference => E.checkType(vr.name) 
    case op: Operation =>
      val left = visit(op.l)
      val right = op.r.map { case (oper, expr) =>visit(expr) }.head
      if(matches(left, right, op.r.map { case (oper, expr) =>oper }.head)){
        left
      } else{
        throw new RuntimeException(s"Type missmatch in operation: $node, $left, $right")
      }
    case num: IntegerLiteral => VariableType("i64", -1)
    case flt: FloatLiteral => VariableType("f64", -1)
    case neg: Negation => visit(neg.r)  //
    case fc: FunctionCall =>
      val paramTypes = fc.param.map(visit) //
      val existingType = E.checkType(fc.variable.name)
      if(existingType == VariableType("void",-1)){
        throw new RuntimeException(s"Function has not been declared yet: $node")
        return VariableType("void",-1)
      } 
      existingType
    case _ => throw new RuntimeException(s"Unsupported node type: $node")
  }


  def visit(node: Statement): VariableType = {
    node match {
      case retStmt: ReturnStatement =>
        VariableType("void", -1)
      case varDecl: VariableDeclaration =>
        val existing_type = E.checkType(varDecl.variable)
        if(existing_type == VariableType("void",-1)){
            E.addVar(varDecl.variable, varDecl.variableType)
        } else{
            throw new RuntimeException(s"Trying to declare already declared variable: $node")
            return VariableType("void",-1)
        }
        val exprType = visit(varDecl.value)
        if(!matches(varDecl.variableType,exprType,"=")){
            throw new RuntimeException(s"Type missmatch in variable declaration: $node, $varDecl.variableType, $exprType")
            
        }
        VariableType("void",-1)
      case varDef: VariableDefinition =>
        val exprType = visit(varDef.value)
        val varType = E.checkType(varDef.variableReference.name)
        if(varType == VariableType("void",-1)){
            throw new RuntimeException(s"Variable has not been declared yet: $node")
            return VariableType("void",-1)
        } 
        if(!matches(varType, exprType,"=")){
            throw new RuntimeException(s"Type missmatch in variable redefinition: $node, $varType, $exprType")
            return VariableType("void",-1)
        }
        VariableType("void",-1)

      case funcDecl: FunctionDeclaration =>
        if(F.checkType(funcDecl.variable) == VariableType("void",0)){
            val params = funcDecl.param.map { case (varType,varRef) => s"${visit(varType)} ${visit(varRef)}" }
            F.addFunc(funcDecl.variable, FunctionSignature(funcDecl.variableType, funcDecl.param))
        } else{
            throw new RuntimeException(s"Trying to declare already declared function: $node")
            return VariableType("void",-1)
        }
        // Step into new scope for function
        
        val body = funcDecl.body.map(visit)

        funcDecl.variableType
      // Add other cases for different Statement types
      case cond: Conditional =>
        val condition = visit(cond.condition.getOrElse(throw new RuntimeException(s"Type missmatch in conditional: $node")))
        val body = cond.body.map(visit)
        val next = cond.next.map(visit)
        VariableType("void",-1)
      case forLoop: ForLoop =>
        val init = forLoop.ct.map(visit).getOrElse("")
        val condition = forLoop.condition.map(visit).getOrElse(throw new RuntimeException(s"Type missmatch in conditional: $node"))
        val increment = forLoop.redefinition.map(visit).getOrElse("")
        val body = forLoop.body.map(visit)
        VariableType("void",-1)
      case whileLoop: WhileLoop =>
        val condition = visit(whileLoop.condition)
        val body = whileLoop.body.map(visit).mkString("\n")
        VariableType("void",-1)
      case funcCallStmt: FunctionCallAsStatement =>
        visit(funcCallStmt.func)
      case includeStmt: Include =>
        VariableType("void",-1)
      case _ => VariableType("void",-1)
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


  def visit(node: Program): VariableType = {
    node.statements.map(visit)
    VariableType("void",-1)
  }
}
