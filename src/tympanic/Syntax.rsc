module tympanic::Syntax

lexical Comment = "//" ![\n]* $;
layout Layout = [\ \t\r\n]* | Comment;
keyword Keywords
  = "mapping"
  | "import"
  | "export"
  | "constructors"
  | "null"
  ;

start syntax Extraction = Header header Import import Export export "types" Datatype+ datatypes "constructors" Constructor+ constructor;

lexical Id = [a-zA-z0-9_]+;
syntax Header = "mapping" Id id;
syntax Import = "import" JavaClassName import;
syntax Export = "export" RascalModule export;

lexical JavaClassName = [a-zA-Z.]+;
lexical RascalADTName = [A-Z][a-zA-Z]*;
lexical RascalConstructorName = [\\]?[a-z][a-zA-Z]*;
lexical RascalModule = [a-zA-Z]+ ("::" [a-zA-Z]+)*;

syntax Datatype = JavaClassName className "=\>" RascalADTName adtName;

lexical FromArgument = [a-zA-Z0-9_\-$]+;
lexical Value = [a-zA-Z0-9\"]+;

syntax FromArg
  = assigned: FromArgument "==" Value
  | nonnull: FromArgument "!=" "null"
  | nullable: FromArgument "?"
  | \default: FromArgument
  ;

syntax ToArg
  = \default: Id
  | typed: RascalADTName adt Id fieldName "=" Id constructorName
  ;  

syntax From = JavaClassName className {FromArg ","}* fromArgs; 

syntax To = RascalConstructorName constructorName "(" {ToArg ","}* toArgs ")";

syntax Constructor = From from ":" To to;
