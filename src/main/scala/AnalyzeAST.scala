package com.github.markrenzi.dsl

trait TypeVisitor {
  def visit(node: Expression): VariableType
  def visit(node: Statement): VariableType
  def visit(node: Program): VariableType
  def visit(node: VariableType): String
}

class Environment(val parent: Option[Environment]){ //TODO NEW SCOPE STUFF
    var isEmpty = true
    private var E:Map[String, VariableType] = Map()

    def addVar(id: String, vartype: VariableType): Unit = 
        E += (id -> vartype)
        isEmpty = false
    
    def checkType(id: String): VariableType = 
        if( E.contains(id)) {
            return E(id)
        } else {
            return parent match {
                case Some(p) => p.checkType(id)
                case None => VariableType("void", 0)
            }
        }
    
    def getParent(): Environment = 
        return parent match {
            case Some(p) => p
            case None => new Environment(None)
        }

}

class FunctionSignature(retType: VariableType,params : Seq[(VariableType, VariableReference)]){
    var returnType = retType
    var paramList = params
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
            return F(id).paramList
        } else {
            return Seq[(VariableType, VariableReference)]()
        }

}


class ASTAnalyzer extends TypeVisitor {
    var E = new Environment(None)
    var F = new FunctionEnvironment()
    var Ep = E

  def matches(t1: VariableType, t2: VariableType, op: String): Boolean = {
    val equalTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64", "bool", "string")
    val arithmeticTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64", "f32", "f64")
    val shiftTypes:List[String] = List("u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64")
    val logicalTypes:List[String] = List("bool")
    val stringTypes:List[String] = List("string")
    val floatTypes:List[String] = List("f32", "f64")
    val targetTypes:List[String] = List("I2CTarget")
    
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
            (shiftTypes.contains(t1.t) && shiftTypes.contains(t2.t)) || (logicalTypes.contains(t1.t) && logicalTypes.contains(t2.t)) || (floatTypes.contains(t1.t) && floatTypes.contains(t2.t)) || (stringTypes.contains(t1.t) && stringTypes.contains(t2.t)) || ((targetTypes.contains(t1.t) && targetTypes.contains(t2.t)))
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
        VariableType("void", 0) //
    case vr: VariableReference => Ep.checkType(vr.name)
    case op: Operation =>
      val left = visit(op.l)
      val right = op.r.map { case (oper, expr) =>visit(expr) }.head
      if(matches(left, right, op.r.map { case (oper, expr) =>oper }.head)){
        resultType(left, right, op.r.map { case (oper, expr) =>oper }.head)
      } else{
        throw new RuntimeException(s"Type mismatch in operation: $node, $left, $right")
      }
    case num: IntegerLiteral => VariableType("i64", -1)
    case flt: FloatLiteral => VariableType("f64", -1)
    case neg: Negation => visit(neg.r)  //
    case fc: FunctionCall =>

      val paramTypes = fc.param.map(visit) //
      val existingType = F.checkType(fc.variable.name)
      if(existingType == VariableType("void", 0)){
        throw new RuntimeException(s"Function has not been declared yet: $node")
        return VariableType("void", 0)
      } 
      existingType
    case tr: TargetReference => VariableType("I2CTarget", -1)
    case tq: TargetQuery => VariableType("i32", -1)
    case _ => throw new RuntimeException(s"Unsupported node type: $node")
  }


  def visit(node: Statement): VariableType = {
    node match {
      case retStmt: ReturnStatement =>
        VariableType("void", 0)
      case varDecl: VariableDeclaration =>
        val existing_type = Ep.checkType(varDecl.variable)
        if(existing_type == VariableType("void", 0)){
            Ep.addVar(varDecl.variable, varDecl.variableType)
        } else{
            throw new RuntimeException(s"Trying to declare already declared variable: $node")
            return VariableType("void", 0)
        }
        val exprType = visit(varDecl.value)
        if(!matches(varDecl.variableType,exprType,"=")){
            throw new RuntimeException(s"Type mismatch in variable declaration: $node, ${varDecl.variableType}, $exprType")
            
        }
        VariableType("void", 0)
      case varDef: VariableDefinition =>
        val exprType = visit(varDef.value)
        val varType = Ep.checkType(varDef.variableReference.name)
        if(varType == VariableType("void", 0)){
            throw new RuntimeException(s"Variable has not been declared yet: $node")
            return VariableType("void", 0)
        } 
        if(!matches(varType, exprType,"=")){
            throw new RuntimeException(s"Type mismatch in variable redefinition: $node, $varType, $exprType")
            return VariableType("void", 0)
        }
        VariableType("void", 0)

      case funcDecl: FunctionDeclaration =>
        if(funcDecl.variable == "queryI2C"){
            throw new RuntimeException(s"Function name ${funcDecl.variable} reserved already: $node")
        }
        if(F.checkType(funcDecl.variable) == VariableType("void", 0)){
            val params = funcDecl.param.map { case (varType,varRef) => s"${visit(varType)} ${visit(varRef)}" }
            F.addFunc(funcDecl.variable, new FunctionSignature(funcDecl.variableType, funcDecl.param))
        } else{
            throw new RuntimeException(s"Trying to declare already declared function: $node")
            return VariableType("void", 0)
        }
        // Step into new scope for function
        Ep = new Environment(Option(E))
        funcDecl.param.foreach(tup => Ep.addVar(tup._2.name, tup._1))
        val body = funcDecl.body.map(visit)
        Ep = Ep.getParent()
        funcDecl.variableType
      // Add other cases for different Statement types
      case cond: Conditional =>
        val condition = cond.condition match {
            case Some(c) => visit(c)
            case None => VariableType("bool",-1)
        }
        
        if(condition != VariableType("bool",-1)){
            throw new RuntimeException(s"Condtional was not bool: $node, $condition")
        }
        Ep = new Environment(Option(Ep))    // create child environment
        val body = cond.body.map(visit)
        Ep = Ep.getParent()                 // pop to child's parent
        val next = cond.next.map(visit)
        VariableType("void", 0)
      case forLoop: ForLoop =>
        Ep = new Environment(Option(Ep))
        val init = forLoop.ct.map(visit)
        val condition = forLoop.condition match {
            case Some(c) => visit(c)
            case None => VariableType("bool",-1)
        }
        if(condition != VariableType("bool",-1)){
            throw new RuntimeException(s"Condtional was not bool: $node, $condition")
        }
        val increment = forLoop.redefinition.map(visit)
        val body = forLoop.body.map(visit)
        Ep = Ep.getParent()
        VariableType("void", 0)
      case whileLoop: WhileLoop =>
        val condition = visit(whileLoop.condition)
        Ep = new Environment(Option(Ep))
        val body = whileLoop.body.map(visit)
        Ep = Ep.getParent()
        VariableType("void", 0)
      case funcCallStmt: FunctionCallAsStatement =>
        visit(funcCallStmt.func)
      case includeStmt: Include =>
        VariableType("void", 0)
      case _ => VariableType("void", 0)
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
      case "I2CTarget" => "uint32_t"
      case _ => throw new RuntimeException(s"Unsupported type: ${node.t}")
    }

    val arrayDimensions = if (node.arr != -1) "[]" * node.arr else ""
    s"$baseType$arrayDimensions"
  }


  def visit(node: Program): VariableType = {
    node.statements.map(visit)
    VariableType("void", 0)
  }
}
