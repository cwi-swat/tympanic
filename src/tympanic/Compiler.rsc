module tympanic::Compiler

import IO;
import List;
import Map;
import ParseTree;
import Set;
import ValueIO;

import lang::java::m3::Core;
import lang::java::m3::TypeSymbol;
import tympanic::Syntax;

set[Id] adtIds = {};
set[str] adtNames = {};
map[Id, loc] javaIds = ();
set[loc] topLevels = {};
rel[loc, loc] ex = {};
map[Id, str] idToStr = ();
rel[str, str] javaNtToCtor = {};
M3 m3 = m3(|unknown:///|);


M3 getM3s(Import* imports) {
  return readM3(cdtM3loc);
}

set[str] extractAdts(Datatype* types) = { "<adt>" | (Datatype)`<Id _> =\> <Id adt>` <- types};

map[Id,loc] javaIdToLoc(ASTMapping mapping, M3 m3) {
  set[loc] candidates = {l|l<-(m3.containment*)[|java+package:///org/eclipse/cdt/core/dom/ast|], l.scheme in {"java+class", "java+interface"}};
  map[Id,loc] ret = ();
  set[Id] javaIds = {datatype.javaType | datatype <- mapping.datatypes} + {datatype.javaType | datatype <- mapping.mappings};
  for (Id id <- javaIds) {
    matches = [candidate | loc candidate <- candidates, candidate.file == "<id>"];
    if (size(matches)!=1) {
      throw "Error looking up Java Type <id> in m3";
    }
    ret += (id : matches[0]);
  }
  return ret;
}

loc getNonterminal(loc class) {
  for (loc nt <- topLevels) {
    if (<class, nt> <- ex) {
      return nt;
    }
  }
  throw "Could not decide for <class>";
}

loc fieldToRascalType(JavaField field, ex){
  visit(field) {
    case foo: ;
  }
}

str makeArg({method(_,_,TypeSymbol ts, _)}) = makeArg({ts});
str makeArg({interface(loc l, _)}) = "<idToStr[invertUnique(javaIds)[getNonterminal(l)]]>";
str makeArg({array(TypeSymbol ts, _)}) = "list[<makeArg({ts})>]";
str makeArg({TypeSymbol::\int()}) = "int";

void fillRelations(ASTMapping astMapping, M3 m3model) {
  m3 = m3model;
  adtIds = {typ.javaType | typ <- astMapping.datatypes};
  adtNames = extractAdts(astMapping.datatypes);
  javaIds = javaIdToLoc(astMapping, m3);
  topLevels = {javaIds[bla] | bla <- adtIds};
  ex = m3.extends*;
  idToStr = (typ.javaType : "<typ.adt>" | typ <- astMapping.datatypes);
}

void compileADT(ASTMapping astMapping, M3 m3model) {
  fillRelations(astMapping, m3model);
  
  rel[str, str] adts = {};
  for (Mapping mapping <- astMapping.mappings) {
    args = [];
    loc nonterminal = getNonterminal(javaIds[mapping.javaType]);
    int i = 0;
    for (field:(JavaField)`<Field _>` <- mapping.fields) {
      rascalArg = mapping.constructor.args[i];
      if ((Arg)`<Id _> <Id _> = <RascalValue _>` := rascalArg) {
        args += "<rascalArg.\type> <rascalArg.name>";
      } else {
        argName = "<rascalArg.name>";
        fieldLoc = getJavaField(field.field.id, javaIds[mapping.javaType]);
        argType = makeArg(m3.types[fieldLoc]);
        if ((Field)`<Id _>?` := field.field) {
          args += "Maybe[<argType>] <argName>";
        } else {
          args += "<argType> <argName>";
        }
      }
      i = i + 1;
    }
    adts += <idToStr[invertUnique(javaIds)[nonterminal]], "<mapping.constructor.name>(<intercalate(", ", args)>)">;
  }
  printAdts(compileAdditionalDatatypes(astMapping));
  printAdts(adts);
}

void printAdts(rel[str, str] adts) {
  for (dt <- adts<0>) {
    list[str] ctors = sort(adts[dt]);
    println("data <dt> 
            '  = <intercalate("\n| ", ctors)>
            '  ;");
  }
}

rel[str, str] compileAdditionalDatatypes(ASTMapping astMapping) = {<"<arg.\type>","<arg.\value>"> | /arg:(Arg)`<Id typ> <Id _> = <RascalValue val>` := astMapping}; 

loc getJavaField(Id field, loc class) {
  locs = m3.containment[(m3.extends*)[class]];
  for (loc l <- locs) {
    if (l.file == "<field>()") {
     return l;
    }
  }
  throw "Failed on <field> and <class>";
}

void compileMarshaller(ASTMapping astMapping, M3 m3model) {
  fillRelations(astMapping, m3model);
  str marshaller
    = "class Marshaller {
      '  private static TypeStore typestore = new TypeStore();
      '  private static TypeFactory tf = 
      '<for (str adtName <- adtNames) {>  public static IConstructor map(<adtName> node) {
      '    return new Marshaller().visit(node);
      '  }
      '<}>
      '<for (str adtName <- adtNames) {>  public IConstructor visit(<adtName> node) {
      '    throw new RuntimeException(\"Encountered unknown <adtName> subclass \" + node.getClass().getSimpleName());
      '  }
      '<}>
      '}";
  println(marshaller);
}
