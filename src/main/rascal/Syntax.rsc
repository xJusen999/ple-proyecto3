module Syntax

// Programa y módulos
start syntax Program = program: Module+ modules ; 

syntax Module
= funcDef: FunctionDef
| dataDecl: Data
; 
// Spec-compliant Data declarations
syntax Type
  = intType:    "Int"
  | boolType:   "Bool"
  | charType:   "Char"
  | stringType: "String"
  | userType:   Id          // tipos definidos por el usuario con 'data'
  ;

syntax FieldDecl = fieldDecl: Id ":" Type;

syntax FieldDecls = fieldDecls: FieldDecl ("," FieldDecl)*;

// Spec-compliant Data declarations
syntax Data
  // FORMAS ORIGINALES (compatibles con el Proyecto 2)
  = dataWithAssign:
        Id assignName "=" "data" "with" Variables vars DataBody body "end" Id endName

  | dataNoAssign:
        "data" "with" Variables vars DataBody body "end" Id endName

  // NUEVAS FORMAS TIPADAS (Proyecto 3)

  // Ejemplo:
  //   PointVar : Point = data with x : Int, y : Int
  //                 Point = struct (x, y)
  //              end Point
  | dataWithAssignTyped:
        Id assignName ":" Type dataType
        "=" "data" "with" FieldDecls fields DataBody body "end" Id endName

  // Ejemplo:
  //   data with x : Int, y : Int
  //        Point = struct (x, y)
  //   end Point : Point
  | dataNoAssignTyped:
        "data" "with" FieldDecls fields DataBody body "end" Id endName ":" Type dataType
  ;


syntax DataBody = consBody: Constructor | funcBody: FunctionDef;

syntax Constructor = constructor: Id name "=" "struct" "(" Variables vars ")";

// (legacy DataAbstraction removed)

// Funciones
syntax FunctionDef =
functionDef: "function" Id name "(" {Id ","}* params ")"
"do" Statement* body
"end" Id endName ; 

syntax ParameterList = parameterList: Id ("," Id)* ; 

// Sentencias
syntax Statement
= assignStmt: Id varName "=" Expression val 
| conditionalStmt: ConditionalStmt ifs 
| loopStmt: LoopStmt loop 
// New statements per grammar
| invokeStmt: Invocation inv
| iteratorStmt: Id varName "=" "iterator" "(" {Id ","}* inVars ")" "yielding" "(" {Id ","}* outVars ")"
| rangeStmtWithVar: Id varName "=" "from" Principal fromP "to" Principal toP
| rangeStmtBare: "from" Principal fromP "to" Principal toP
; 

// Variables list (for invocations/iterators)
syntax Variables = variables: Id ("," Id)* ; // retained for other uses if needed

// Invocation forms
syntax Invocation
= dollarInvoke: Id name "$" "(" {Id ","}* vars ")"
| methodInvoke: Id recv "." Id method "(" {Id ","}* vars ")"
; 

// Constructores
syntax DataConstruction
= dataConstruction: ConstructorCall ; 

syntax ConstructorCall =
ctorCall: "sequence" "[" {Expression ","}* "]"
| ctorCall: "tuple" "(" {Expression ","}* ")"
| ctorCall: "struct" "(" {Expression ","}* args ")" ; 

syntax NamedArg = namedArg: Id name ":" Expression expr ; 

// Condicionales
syntax ConditionalStmt
= ifStmt: IfStmt
| condStmt: CondStmt
; 

syntax IfStmt =
ifStmt: "if" Expression cond "then" Statement* thenBlock
("elseif" Expression "then" Statement*)* elseifBlocks
("else" Statement* elseBlock)?
"end" ; 

syntax CondStmt =
condStmt: "cond" Expression cond "do" CondClause+ clauses "end" ; 

syntax CondClause = condClause: Expression cond "-\>" Statement+ body ;


// Bucles
syntax LoopStmt =
forRange: "for" Id var "from" Expression fromExpr "to" Expression toExpr "do" Statement* body "end" 
| forIn: "for" Id var "in" Expression expr "do" Statement* body "end"
; 

// Jerarquía de expresiones 
syntax Expression = orExpr: OrExpr expr ; 

// Lógica booleana OR 
syntax OrExpr
= andExpr: AndExpr expr
| left binaryOr: OrExpr left "or" AndExpr right
; 

// Lógica booleana AND 
syntax AndExpr
= cmpExpr: CmpExpr expr
| left binaryAnd: AndExpr left "and" CmpExpr right
; 

// Comparación 
syntax CmpExpr
= addExpr: AddExpr expr
| non-assoc binaryExpr: AddExpr left CmpOp op AddExpr right
; 

lexical CmpOp = "\<" | "\>" | "\<=" | "\>=" | "\<\>" | "=" ; 

// Suma/Resta 
syntax AddExpr
= mulExpr: MulExpr expr
| left binaryAdd: AddExpr left AddOp op MulExpr right
; 

lexical AddOp = "+" | "-" ;

// Multiplicación/División/Módulo 
syntax MulExpr
= powExpr: PowExpr expr
| left binaryMul: MulExpr left MulOp op PowExpr right
; 

lexical MulOp = "*" | "/" | "%" ;

// Potencia 
syntax PowExpr
= unaryExpr: UnaryExpr expr
| right binaryPow: UnaryExpr left "**" PowExpr right
; 

// Expresiones unarias
syntax UnaryExpr
= postfix: Postfix postfixExpr
| unaryNeg: "neg" UnaryExpr operand
| unaryMinus: "-" UnaryExpr operand
; 

// Postfix 
syntax Postfix
  = primary: Primary primaryExpr
; 

// Expresiones primarias 
syntax Primary
= bracket groupExpr: "(" Expression expr ")" 
> literalExpr: Literal lit 
> varExpr: Id name 
| ctorExpr: ConstructorCall ctor 
| invExpr: Invocation inv
; 

// Principals (used in ranges, etc.)
syntax Principal
= pTrue: "true"
| pFalse: "false"
| pChar: Char
| pInt: Integer
| pFloat: Float
| pId: Id
;

// Literales
syntax Literal
  = floatLit: Float realValue
  | intLit: Integer intValue
  > boolLit: BooleanLit boolValue
  | charLit: Char charValue
  | stringLit: String strValue
  ;

// Tokens Léxicos

// Boolean literals
lexical BooleanLit = "true" | "false" ;

// Identificadores (excludes reserved words including boolean literals)
lexical Id = [a-zA-Z_][a-zA-Z0-9_\-]* \ Reserved 
           \ "true"
           \ "false" ; 

// Números
lexical Float = [0-9]+ "." [0-9]+ ;

lexical Integer = [0-9]+ ;

// Caracteres
lexical Char = [\'] CharContent [\'] ; 

lexical CharContent
= "\\\\"
| [\'][\\][\']
| "\\n"
| "\\t"
| "\\r"
| ![\'\\]
; 

// Cadenas de texto
lexical String = [\"] StringContent* [\"] ; 

lexical StringContent
= "\\\\"
| [\"][\\][\"]
| "\\n"
| "\\t"
| "\\r"
| ![\"\\]
; 


// Palabras reservadas 
keyword Reserved =
"data" | "with" | "rep" | "struct" | "function" | "do" | "end"
| "if" | "then" | "elseif" | "else" | "cond" | "for" | "from" | "to" | "in"
| "iterator" | "yielding" | "sequence" | "tuple"
| "and" | "or" | "neg" | "true" | "false" ;

// Espacios y comentarios
layout Layout = WhitespaceOrComment* !>> [\ \t\n\r#];

lexical WhitespaceOrComment
= [\ \t\n\r]
| Comment
; 

lexical Comment = "#" ![\n\r]* ; 