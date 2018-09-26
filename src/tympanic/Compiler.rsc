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

bool relationsFilled = false;
set[Id] adtIds = {};
set[str] adtNames = {};
map[Id, loc] javaIds = ();
map[str, loc] javaStrs = ();
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
  set[Id] javaIds
    = {datatype.javaType | datatype <- mapping.datatypes}
    + {datatype.javaType | datatype <- mapping.mappings}
    + {castType | /(Field)`(<Id castType>) <Id _>` := mapping}
    + {castType | /(Field)`(<Id castType>[]) <Id _>` := mapping};
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

map[str,loc] javaStrToLoc(ASTMapping mapping, M3 m3) {
  set[loc] candidates = {l|l<-(m3.containment*)[|java+package:///org/eclipse/cdt/core/dom/ast|], l.scheme in {"java+class", "java+interface"}};
  map[str,loc] ret = ();
  set[Id] javaIds = {datatype.javaType | datatype <- mapping.datatypes} + {datatype.javaType | datatype <- mapping.mappings};
  for (Id id <- javaIds) {
    matches = [candidate | loc candidate <- candidates, candidate.file == "<id>"];
    if (size(matches)!=1) {
      throw "Error looking up Java Type <id> in m3";
    }
    ret += ("<id>" : matches[0]);
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

str makeArg({method(_,_,TypeSymbol ts, _)}) = makeArg({ts});
str makeArg({interface(loc l, _)}) = "<idToStr[invertUnique(javaIds)[getNonterminal(l)]]>";
str makeArg({array(TypeSymbol ts, _)}) = "list[<makeArg({ts})>]";
str makeArg({TypeSymbol::\int()}) = "int";
str makeArg({TypeSymbol::\boolean()}) = "bool";
str makeArg({class(loc l,_)}) = loc l := |java+class:///java/lang/String| ? "str" : makeArg(l);
default str makeArg(value v) {
  println("<v>");
  throw "<v>";
}

void fillRelations(ASTMapping astMapping, M3 m3model) {
  if (relationsFilled) {
    return;
  }
  m3 = m3model;
  adtIds = {typ.javaType | typ <- astMapping.datatypes};
  adtNames = extractAdts(astMapping.datatypes);
  javaIds = javaIdToLoc(astMapping, m3);
  javaStrs = javaStrToLoc(astMapping, m3);
  topLevels = {javaIds[bla] | bla <- adtIds};
  ex = m3.extends*;
  idToStr = (typ.javaType : "<typ.adt>" | typ <- astMapping.datatypes);
  javaNtToCtor = {<idToStr[invertUnique(javaIds)[getNonterminal(javaIds[rule.javaType])]], "<rule.javaType>"> | /Mapping rule := astMapping};
  relationsFilled = true;
}

str compileADT(ASTMapping astMapping, M3 m3model) {
  //fillRelations(astMapping, m3model);
  
  rel[str, str] adts = {};
  for (Mapping mapping <- astMapping.mappings) {
    args = [];
    loc nonterminal = getNonterminal(javaIds[mapping.javaType]);
    int i = 0;
    list[Arg] rascalArgs = ([] | it + arg | Arg arg <- mapping.constructor.args);
    for (field:(JavaField)`<Field _>` <- mapping.fields) {
      rascalArg = rascalArgs[i];
      if ((Arg)`<Id _> <Id _> = <RascalValue _>` := rascalArg) {
        args += "<rascalArg.\type> \\<rascalArg.name>";
      } else {
        argName = "\\<rascalArg.name>";
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
    adts += <idToStr[invertUnique(javaIds)[nonterminal]], "\\<mapping.constructor.name>(<intercalate(", ", args)>)">;
  }
  for (str adt <- adts<0>) {
    adts += <adt, "________error(str fileLocation)">;
    adts += <adt, "________null()">;
  }
  return "module <astMapping.export>::Data
         '
         'import util::Maybe;
         '<printAdts(compileAdditionalDatatypes(astMapping))>
         '<printAdts(adts)>";
}

str printAdts(rel[str, str] adts) {
  str ret = "";
  for (dt <- adts<0>) {
    list[str] ctors = sort(adts[dt]);
    ret += "data <dt>
           '  = <intercalate("\n| ", ctors)>
           '  ;\n";
  }
  return ret;
}

rel[str, str] compileAdditionalDatatypes(ASTMapping astMapping) = {<"<arg.\type>","\\<arg.\value>"> | /arg:(Arg)`<Id typ> <Id _> = <RascalValue val>` := astMapping}; 

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
    case method(getterLoc, _, TypeSymbol::\boolean(), _) : {
      return "boolean";
    }
    case method(getterLoc, _, enum(loc enumLoc), _) : {
      return unescapeJavaType(enumLoc);
    }
    case method(getterLoc, _, class(loc classLoc, _), _) : {
      if (classLoc := |java+class:///java/lang/String|) {
        return "String";
      }
      fail;
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
  list[Arg] rascalArgs = ([] | it + arg | Arg arg <- mapping.constructor.args);
  list[JavaField] javaFields = ([] | it + jf | jf:(JavaField)`<Field _>` <- mapping.fields);
  for (Arg arg <- rascalArgs) {
    JavaField jf = javaFields[i];
    loc jfloc = getJavaField(jf.field.id, mloc);
    str javaArgType = getJavaArgType(jfloc);
    if ((Arg)`<Id typ> <Id name> = <RascalValue val>` := rascalArgs[i]) {
      ret += "IConstructor $arg<i> = vf.constructor(_<typ>_<"<val>"[0..-2]>);\n"; //TODO use val.id
    } else if (/\[\]$/ := javaArgType && (JavaField)`<Id _>?` := jf) {
      ret += "IListWriter $arg<i>_list = vf.listWriter();
             'if (((<mapping.javaType>) node).<jf.field.id>() != null) {
             '  for (<javaArgType[0..-2]> $$arg<i> : ((<mapping.javaType>) node).<jf.field.id>()) {
             '    $arg<i>_list.append(visit($$arg<i>));
             '  }
             '}
             'IList $arg<i> = $arg<i>_list.done();\n";
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
    } else if (javaArgType == "boolean") {
      ret += "IBool $arg<i> = vf.bool(((<mapping.javaType>) node).<jf.field.id>());\n";
    } else if (javaArgType == "String") {
      ret += "IString $arg<i> = vf.string(((<mapping.javaType>) node).<jf.field.id>());\n";
    } else {
      ret += "IConstructor $arg<i> = visit(((<mapping.javaType>) node).<jf.field.id>());\n";
    }
    i = i + 1;
  }
  ret += "return vf.constructor(_<idToStr[adtName]>_<mapping.constructor.name><("" | "<it>, $arg<n>" | n <- [0..i])>);";
  return ret;
}

str getArgumentType(Field field, Arg arg, Mapping mapping) {
  if ((Arg)`<Id typ> <Id _> = <RascalValue _>` := arg){ 
    return "_<typ>";
  } else {
    str typ = "<makeArg(m3.types[getJavaField(field.id, javaIds[mapping.javaType])])>";
    switch (typ) {
      case /list\[<t:.*>\]/ : return "tf.listType(_<t>)";
      case "bool" : return "tf.boolType()";
      case "str" : return "tf.stringType()";
      default: return "_<typ>";
    }
  }
}

str declareTypes(ASTMapping astMapping, M3 m3model) {
  str ret = "";
  set[str] handled = {};
  for (Id adtName <- idToStr) {
    for (/Mapping mapping <- astMapping.mappings, getNonterminal(javaIds[mapping.javaType]) == javaIds[adtName], "_<idToStr[adtName]>_<mapping.constructor.name>" notin handled) {
      handled += "_<idToStr[adtName]>_<mapping.constructor.name>";
      list[Field] fields = ([] | it + f | (JavaField)`<Field f>` <- mapping.fields);
      list[Arg] args = ([] | it + arg | arg <- mapping.constructor.args);
      list[tuple[Field,Arg]] argMap = [<fields[i], args[i]> | i <- [0..size(fields)]];
      int i = 0;
      ret += "private static final Type _<idToStr[adtName]>_<mapping.constructor.name>
             '  = tf.constructor(typestore, _<idToStr[adtName]>, \"<mapping.constructor.name>\"<("" | "<it>, <getArgumentType(field, arg, mapping)>, \"<arg.name>\"" | <field, arg> <- argMap)>);\n";
    }
    ret += "\n";
  }
  for (str adtName <- range(idToStr)) {
    ret += "private static final Type _<adtName>_________error
           '  = tf.constructor(typestore, _<adtName>, \"________error\", tf.stringType(), \"fileLocation\");
           'private static final Type _<adtName>_________null
           '  = tf.constructor(typestore, _<adtName>, \"________null\");\n";
  }
  ret += "\n";
  return ret;
}

str declareAdditionalDatatypes(ASTMapping astMapping) {
  str ret = "";
  set[str] handled = {};
  for (Id adt <- {typ | /(Arg)`<Id typ> <Id _> = <RascalValue _>` := astMapping}, "<adt>" notin handled) {
    handled += "<adt>";
    ret += "private static final Type _<adt> = tf.abstractDataType(typestore, \"<adt>\");\n";
    for (/(Arg)`<Id typ> <Id _> = <Id cname> ( <{RascalValue ","}* _> )` := astMapping, typ == adt) {
      ret += "private static final Type _<adt>_<cname>
             '  = tf.constructor(typestore, _<adt>, \"<cname>\");\n";
    }
  }
  return ret;
}

str compileMarshaller(ASTMapping astMapping, M3 m3model) {
  //fillRelations(astMapping, m3model);
  str marshaller
    = "package <replaceAll("<astMapping.export>.internal", "::", ".")>; 
      '
      'import io.usethesource.vallang.*;
      'import io.usethesource.vallang.type.*;
      'import org.rascalmpl.interpreter.IEvaluatorContext;
      '
      '<for (loc l <- sort(range(javaIds))) {>import <unescapeJavaType(l)>;
      '<}>
      '
      'public class Marshaller {
      '  private static TypeStore typestore = new TypeStore();
      '  private static TypeFactory tf = TypeFactory.getInstance();
      '  private IValueFactory vf;
      '  private IEvaluatorContext ctx;
      '
      '  public Marshaller(IValueFactory vf, IEvaluatorContext ctx) {
      '    this.vf = vf;
      '    this.ctx = ctx;
      '  }
      '
      '<for (str adtName <- range(idToStr)) {>  private static final Type _<adtName> = tf.abstractDataType(typestore, \"<adtName>\");
      '<}>
      '  <declareAdditionalDatatypes(astMapping)>
      '
      '  <declareTypes(astMapping, m3model)> 
      '  private static final Type _Maybe = tf.abstractDataType(typestore, \"Maybe\");
      '  private static final Type _Maybe_nothing
      '    = tf.constructor(typestore, _Maybe, \"nothing\");
      '  private static final Type _Maybe_just
      '    = tf.constructor(typestore, _Maybe, \"just\", tf.parameterType(\"A\"), \"val\");
      '
      '<for (Id adtName <- idToStr) {>  public IConstructor map(<adtName> node) {
      '    return new Marshaller(vf, ctx).visit(node);
      '  }
      '<}>
      '<for (Id adtName <- idToStr) {>  public IConstructor visit(<adtName> node) {
      '    if (node == null) {
      '      return vf.constructor(_<idToStr[adtName]>_________null);
      '    }
      '<for (/Mapping mapping <- astMapping.mappings, javaIds[mapping.javaType] in {l | ctor <- javaNtToCtor[idToStr[adtName]], loc l := javaStrs[ctor]/*, <l,javaIds[adtName]> in ex*/}) {>    if (node instanceof <mapping.javaType>) {
      '      if (<makeConstructorGuard(mapping)>) {
      '        <makeConstructor(astMapping, mapping, adtName)>
      '      }
      '    }
      '<}>
      '    ctx.getStdErr().println(\"Encountered unknown <adtName> subtype \" + node.getClass().getSimpleName() + \" at \" + node.getFileLocation());
      '    return vf.constructor(_<idToStr[adtName]>_________error, vf.string(node.getFileLocation().toString()));
      '  }
      '<}>
      '}";
  //println(marshaller);
  return marshaller;
}

void checker(ASTMapping astMapping) {
  fillRelations(astMapping, m3model);
  bool foundError = false;
  for (Mapping mapping <- astMapping.mappings) {
    //Argument count mismatch
    //TODO: account for skipped arguments on lhs
    int javaFields = (0 | it + 1 | JavaField _ <- mapping.fields);
    int rascalArgs = (0 | it + 1 | Arg _ <- mapping.constructor.args);
    if (javaFields != rascalArgs) {
      println("Argument count mismatch (lhs: <javaFields>, rhs: <rascalArgs>) for
              '  <mapping>");
      foundError = true;
    }
  }
  if (foundError) {
    throw "Found errors, aborting";
  }
}

void doCompile(ASTMapping astMapping, M3 m3model) {
  fillRelations(astMapping, m3model);
  checker(astMapping);
  str dataFile = compileADT(astMapping, m3model);
  str marshallerFile = compileMarshaller(astMapping, m3model);
  loc dataLoc = |project://tympanic/src| + replaceAll("<astMapping.export>/Data.rsc", "::", "/");
  loc marshallerLoc = |project://tympanic/src| + replaceAll("<astMapping.export>/internal/Marshaller.java", "::", "/");
  writeFile(dataLoc, dataFile);
  writeFile(marshallerLoc, marshallerFile);
}
