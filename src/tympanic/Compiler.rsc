module tympanic::Compiler

import IO;
import List;
import Map;
import ParseTree;
import Set;
import String;
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
  //for (Id i <- ret) println("<i> : <ret[i]>");
  //return ();
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
  javaNtToCtor = {<idToStr[invertUnique(javaIds)[getNonterminal(javaIds[rule.javaType])]], "<rule.javaType>"> | /Mapping rule := astMapping};
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
  println("module <astMapping.export>::Data\n");
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

void printRelations(ASTMapping astMapping, M3 m3model) {
  fillRelations(astMapping, m3model);
  println("adtIds: {\n<for (i<-adtIds){>  <i>\n<}>}");
  println("adtNames: " + itoString(adtNames));
  println("javaIds: (\n<for (k<-javaIds){>  <k> : <javaIds[k]>\n<}>)");
  println("topLevels: " + itoString(topLevels));
  //println("ex: " + itoString(ex));
  println("idToStr: (\n<for (k<-idToStr){>  <k> : <idToStr[k]>\n<}>)");
  println("javaNtToCtor: " + itoString(javaNtToCtor));
}

str unescapeJavaType(loc javaType) = replaceAll(javaType.path[1..], "/", ".");

str getJavaArgType(loc getterLoc) {
  TypeSymbol getter = [*m3.types[getterLoc]][0];
  switch(getter) {
    case method(getterLoc, _, interface(loc returnType, _), _) : {
      return unescapeJavaType(returnType);
    }
    case method(getterLoc, _, array(interface(loc returnType, _), _), _) : {
      return unescapeJavaType(returnType)+"[]";
    }
    case method(getterLoc, _, TypeSymbol::\int(), _) : {
      return "int";
    }
    default: {
      println("NYI\n<"<getter>">");
      throw "";
    }
  }
  
  return "";
}

str makeConstructorGuard(Mapping mapping)
  = (("true" | "<it>
               '&& (((<mapping.javaType>) node).<id>() == <jv>)" | /(Field)`<Id id> == <JavaValue jv>` := mapping.fields)
    | "<it>
      '&& (((<mapping.javaType>) node).<id>() != <jv>)" | /(Field)`<Id id> != <JavaValue jv>` := mapping.fields);


str makeConstructor(ASTMapping astMapping, Mapping mapping, Id adtName) {
  str ret = "";
  loc mloc = javaIds[mapping.javaType];
  int i = 0;
  list[Arg] rascalArgs = [];
  for (Arg arg <- mapping.constructor.args) {
    rascalArgs += arg;
  }
  for (JavaField jf <- mapping.fields) {
    loc jfloc = getJavaField(jf.field.id, mloc);
    str javaArgType = getJavaArgType(jfloc);
    if ((Arg)`<Id typ> <Id name> = <RascalValue val>` := rascalArgs[i]) {
      ret += "IConstructor $arg<i> = vf.constructor(_<typ>_<"<val>"[0..-2]>);\n"; //TODO use val.id
    } else if (/\[\]$/ := javaArgType) {
      ret += "IListWriter $arg<i>_list = vf.listWriter();
             'for (<javaArgType[0..-2]> $$arg<i> : ((<mapping.javaType>) node).<jf.field.id>()) {
             '  $arg<i>_list.append(visit($$arg<i>));
             '}
             'IList $arg<i> = $arg<i>_list.done();\n";
    } else if ((JavaField)`<Id _>?` := jf) {
      ret += "IConstructor $arg<i>;
             '<javaArgType> _$arg<i> = ((<mapping.javaType>) node).<jf.field.id>();
             'if (_$arg<i> == null) {
             '  $arg<i> = vf.constructor(_Maybe_nothing);
             '} else {
             '  $arg<i> = vf.constructor(_Maybe_just, visit(_$arg<i>));
             '}\n";
    } else {
      ret += "IConstructor $arg<i> = visit(((<mapping.javaType>) node).<jf.field.id>());\n";
    }
    i = i + 1;
  }
  ret += "return vf.constructor(_<idToStr[adtName]>_<mapping.javaType><("" | "<it>, $arg<n>" | n <- [0..i])>);";
  return ret;
}

str declareTypes(ASTMapping astMapping, M3 m3model) {
  str ret = "";
  for (str adtName <- range(idToStr)) {
    for (str ctor <- javaNtToCtor[adtName]) {
      ret += "private static final Type _<adtName>_<ctor>
             '  = tf.constructor(typestore, _<adtName>, \"ctor\"<("" | "<it>, tf.valueType(), <argName>" | argName <- ["null", "null"])>);\n";
    }
    ret += "\n";
  }
  return ret;
}

str declareAdditionalDatatypes(ASTMapping astMapping) {
  str ret = "";
  set[str] handled = {};
  for (Id typ <- {typ | /(Arg)`<Id typ> <Id _> = <RascalValue _>` := astMapping}, "<typ>" notin handled) {
    handled += "<typ>";
    ret += "private static final Type _<typ> = tf.abstractDataType(typestore, \"<typ>\");\n";
    for (/(Arg)`<Id typ> <Id _> = <Id cname> ( <{RascalValue ","}* _> )` := astMapping) {
      ret += "private static final Type _<typ>_<cname>
             '  = tf.constructor(typestore, _<typ>, \"<cname>\");\n";
    }
  }
  return ret;
}

void compileMarshaller(ASTMapping astMapping, M3 m3model) {
  fillRelations(astMapping, m3model);
  str package = replaceAll("<astMapping.export>.internal", "::", ".");
  str marshaller
    = "package <package>; 
      '
      'import io.usethesource.vallang.*;
      'import io.usethesource.vallang.type.*;
      '
      '<for (loc l <- range(javaIds)) {>import <unescapeJavaType(l)>;
      '<}>
      '
      'class Marshaller {
      '  private static TypeStore typestore = new TypeStore();
      '  private static TypeFactory tf = TypeFactory.getInstance();
      '  private IValueFactory vf;
      '
      '  public Marshaller(IValueFactory vf) {
      '    this.vf = vf;
      '  }
      '
      '<for (str adtName <- range(idToStr)) {>  private static final Type _<adtName> = tf.abstractDataType(typestore, \"<adtName>\");
      '<}>
      '  <declareTypes(astMapping, m3model)> 
      '  private static final Type _Maybe = tf.abstractDataType(typestore, \"Maybe\");
      '  private static final Type _Maybe_nothing
      '    = tf.constructor(typestore, _Maybe, \"nothing\");
      '  private static final Type _Maybe_just
      '    = tf.constructor(typestore, _Maybe, \"just\", tf.parameterType(\"A\"), \"val\");
      '
      '  <declareAdditionalDatatypes(astMapping)>
      '<for (Id adtName <- idToStr) {>  public IConstructor map(<adtName> node) {
      '    return new Marshaller(vf).visit(node);
      '  }
      '<}>
      '<for (Id adtName <- idToStr) {>  public IConstructor visit(<adtName> node) {
      '<for (/Mapping mapping <- astMapping.mappings, getNonterminal(javaIds[mapping.javaType]) == javaIds[adtName]) {>    if (node instanceof <mapping.javaType>) {
      '      if (<makeConstructorGuard(mapping)>) {
      '        <makeConstructor(astMapping, mapping, adtName)>
      '      }
      '    }
      '<}> 
      '    throw new RuntimeException(\"Encountered unknown <adtName> subtype \" + node.getClass().getSimpleName());
      '  }
      '<}>
      '}";
  println(marshaller);
}
