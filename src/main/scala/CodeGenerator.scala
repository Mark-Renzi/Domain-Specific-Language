package com.github.markrenzi.dsl

trait ASTVisitor {
  def visit(node: Expression): String
  def visit(node: Statement): String
  def visit(node: Program): String
  def visit(node: VariableType): String
}





class CodeGenerator extends ASTVisitor {

  val template = """#include <wiringPiI2C.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <json.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <unistd.h>

#define MAX_BUFFER_SIZE 4096

int connectServer(char *server_ip, char *server_port){
	struct addrinfo hints;
	struct addrinfo *results, *curr;
	memset(&hints, 0, sizeof(struct addrinfo));
	hints.ai_flags = AI_PASSIVE;
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_flags |= AI_NUMERICSERV;
	
	int s;
	int status = getaddrinfo(server_ip, server_port, &hints, &results);
	if(status!=0){
		fprintf(stderr, "Error: %s\n", gai_strerror(status));
	}
	for(curr=results; curr!=NULL; curr=curr->ai_next){
		s = socket(curr->ai_family, curr->ai_socktype, curr->ai_protocol);
		if(s==-1){
			perror("Socket Error");
			continue;
		}
		if(connect(s, curr->ai_addr, curr->ai_addrlen)==0) break;
		perror("Connect Error");
		close(s);
	}
	
	return s;
}

int parseResponse(char* response){
	
	printf("Response:\n%s", response);
	//printf("Header length: %d\n", strlen(response));
	char *pos = strstr(response, "HTTP/1.0 ");
	if (pos != NULL) {
		int status_code = atoi(pos+9);
		if (status_code == 200) {
			printf("Success!\n");
			pos = strstr(response, "Content-Length: ");
			printf("%p\n",pos);
			//int content_length = atoi(pos+16);
			//printf("Content Length: %d\n", content_length);
			//printf("Payload: %s\n", response + (strlen(response)-content_length));
			// TODO- parse server esponse based on return type of execution chain, Client should have knowledge of this
		} else {
			printf("Error: HTTP status code %d\n", status_code);
			return -1;
		}
	}
        
}"""

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
    case tr: TargetReference => s"wiringPiI2CSetup(${tr.value.toString})"
    case tq: TargetQuery => s"wiringPiI2CRead(${tq.variable.name})"
    case cc: ChainCall => 
      val params = cc.param.map(visit).mkString(", ")
      val format_string = s"${cc.server.name}_${cc.variable}_template"
      s"""char message_filled[1024];
      sprintf(message_filled, $format_string, ${params});
      |"""
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
        val body = funcDecl.body.map(visit).mkString("\n\t")
        s"${visit(funcDecl.variableType)} ${funcDecl.variable}($params) {\n\t$body\n}"
      // Add other cases for different Statement types
      case cond: Conditional =>
        val prefix = if(cond.previousIsIf.getOrElse(false)) "else " else ""
        val condition = cond.condition.map(c => s"${prefix}if (${visit(c)})").getOrElse("else")
        val body = cond.body.map(visit).mkString("\n\t")
        val next = cond.next.map(visit).getOrElse("")
        s"$condition {\n\t$body\n} $next"
      case forLoop: ForLoop =>
        val init = forLoop.ct.map(visit).getOrElse("").stripSuffix(";")
        val condition = forLoop.condition.map(visit).getOrElse("")
        val increment = forLoop.redefinition.map(visit).getOrElse("").stripSuffix(";")
        val body = forLoop.body.map(visit).mkString("\n\t")
        s"for ($init; $condition; $increment) {\n\t$body\n}"
      case whileLoop: WhileLoop =>
        val condition = visit(whileLoop.condition)
        val body = whileLoop.body.map(visit).mkString("\n\t")
        s"while ($condition) {\n\t$body\n}"
      case programLoop: ProgramLoop =>
        val body = programLoop.body.map(visit).mkString("\n\t")
        s"while (1) {\n\t$body\n}"
      case funcCallStmt: FunctionCallAsStatement =>
        s"${visit(funcCallStmt.func)};"
      case includeStmt: Include =>
        s"#include \"${includeStmt.path}\""

      case serv: ServerDeclaration=>
        s"uint32_t ${serv.v.name} = connectServer(\"${serv.url}\", \"${serv.port}\");"
      case cd: ChainDeclaration =>
        val params = cd.param.map{case (varType,varRef) => s"${visit(varRef)}"}.mkString("\": %f, \"") + "\": %f"
        val body = cd.body.map(visit).mkString("\n")
        s"""char *${cd.server.name}_${cd.variable}_template = \"{\\\"params\\\": {\\\"${params}}, \\\"body\\\": ${body}}\""""
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
      case "I2CTarget" => "uint32_t"
      case _ => throw new RuntimeException(s"Unsupported type: ${node.t}")
    }

    val arrayDimensions = if (node.arr != -1) "[]" * node.arr else ""
    s"$baseType$arrayDimensions"
  }


  def visit(node: Program): String = {
    template  + "\n" + node.statements.map(visit).mkString("\n")
  }
}
