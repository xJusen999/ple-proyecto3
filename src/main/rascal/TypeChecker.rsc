module TypeChecker

import Syntax;
import ParseTree;
import Message;
import Set;

import analysis::typepal::TModel;
extend analysis::typepal::TypePal;

// -------------------------------------------------------------------
// Tipos del analizador
// -------------------------------------------------------------------

data AType
  = aInt()
  | aBool()
  | aChar()
  | aString()
  | aStruct(str name)
  ;

data IdRole = typeId() | fieldId();

// Pretty printer opcional (útil si activas logs de TypePal)
str prettyPrintAType(aInt())          = "Int";
str prettyPrintAType(aBool())         = "Bool";
str prettyPrintAType(aChar())         = "Char";
str prettyPrintAType(aString())       = "String";
str prettyPrintAType(aStruct(name))   = "Struct(<name>)";

// -------------------------------------------------------------------
// Configuración de TypePal
// -------------------------------------------------------------------

// Mapea nuestros tipos a nombres y roles para las reglas de
// "un use debe referirse a un nombre definido".
tuple[list[str] typeNames, set[IdRole] idRoles]
aluGetTypeNamesAndRole(aStruct(str name)) = <[name], {typeId()}>;
tuple[list[str] typeNames, set[IdRole] idRoles]
aluGetTypeNamesAndRole(AType _)          = <[], {}>;

private TypePalConfig aluConfig(bool debug = false)
  = tconfig(
      getTypeNamesAndRole = aluGetTypeNamesAndRole
      // Si quieres ver más info, puedes descomentar:
      // verbose = debug,
      // logTModel = debug,
      // logAttempts = debug,
      // logSolverIterations = debug
    );

// -------------------------------------------------------------------
// Recolección de hechos
// -------------------------------------------------------------------

void collect(Tree t, Collector c) {
  visit(t) {
    case (Type) ty:
      collectType(ty, c);

    case (Data) d:
      collectData(d, c);
  }
}

void collectType((Type) ty, Collector c) {
  switch (ty) {
    case intType():
      c.fact(ty, aInt());

    case boolType():
      c.fact(ty, aBool());

    case charType():
      c.fact(ty, aChar());

    case stringType():
      c.fact(ty, aString());

    // Tipo de dato definido por el usuario (data ...)
    case userType(Id tp):
      // Verificamos que exista un data con ese nombre (rol typeId)
      c.use(tp, {typeId()});

    default:
      ;
  }
}

// -------------------------------------------------------------------
// Campos declarados / usados (para la Task 4)
// -------------------------------------------------------------------

private set[str] declaredFields(FieldDecls fields) {
  set[str] result = {};

  visit(fields) {
    // fieldDecl: Id ":" Type
    case fieldDecl(Id f, Type _):
      result += { "<f>" };
  }

  return result;
}

private set[str] usedFields(DataBody body) {
  set[str] result = {};

  // Constructor = constructor: Id name "=" "struct" "(" Variables vars ")";
  visit(body) {
    case constructor(Id _, Variables vars):
      visit(vars) {
        case Id f:
          result += { "<f>" };
      }
  }

  return result;
}

// -------------------------------------------------------------------
// Recolección específica para Data + regla nueva de campos
// -------------------------------------------------------------------

void collectData((Data) d, Collector c) {
  switch (d) {
    // data with x : Int, y : Int ... end Point : Point
    case dataNoAssignTyped(FieldDecls fields, DataBody body, Id name, Type dataType):
      handleTypedData(name, dataType, fields, body, d, c);

    // PointVar : Point = data with x : Int, ... end Point
    case dataWithAssignTyped(Id assignName, Type dataType,
                             FieldDecls fields, DataBody body, Id name):
      handleTypedData(name, dataType, fields, body, d, c);

    default:
      ;
  }
}

void handleTypedData(Id name, Type dataType,
                     FieldDecls fields, DataBody body,
                     Data whole, Collector c) {

  // Nombre del tipo, por ejemplo "Point"
  str typeName = "<name>";

  // Definimos el tipo de datos (rol typeId)
  c.define(typeName, typeId(), whole, defType(aStruct(typeName)));

  // Muy importante: seguir recolectando en las partes internas
  collect(dataType, c);
  collect(fields, c);
  collect(body, c);

  // --- Regla nueva (Task 4) ---
  // todos los campos usados en struct(...) deben estar en 'data with ...'
  set[str] declared = declaredFields(fields);
  set[str] used     = usedFields(body);

  for (str f <- used - declared) {
    c.report(
      error(
        whole,
        "Field <f> is used in struct <typeName> but is not declared in the with-clause"
      )
    );
  }
}

// -------------------------------------------------------------------
// Construcción del TModel y función principal
// -------------------------------------------------------------------

public TModel aluTModelForTree(Tree pt, bool debug = false) {
  if (pt has top) {
    pt = pt.top;
  }

  Collector c = newCollector("alu", pt, config = aluConfig(debug = debug));

  collect(pt, c);
  return newSolver(pt, c.run()).run();
}

public list[Message] typeCheckCode(str code, bool debug = false) {
  Tree t = parse(#start[Program], code);

  TModel tm = aluTModelForTree(t, debug = debug);

  if (debug) {
    printTModel(tm);
  }

  return tm.messages;
}
