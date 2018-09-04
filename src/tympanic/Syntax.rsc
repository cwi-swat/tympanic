module tympanic::Syntax

extend lang::std::Layout;
extend lang::std::Id;

start syntax ASTMapping 
  = "mapping" Id
    Import* imports 
    "export" {Id "::"}+ export
    "types" Datatype* datatypes 
    "constructors" Mapping* mappings;

syntax Import = "import" {Id "."}+ import;

syntax Datatype = Id javaType "=\>" Id adt;

syntax Mapping 
  = Id javaType {JavaField ","}* fields ":" Constructor constructor;

syntax JavaField
  = Field field
  | "%" Field field //skipped
  ;

syntax Field
  = Id id
  | Id id "==" JavaValue val
  | Id id "!=" JavaValue val
  | Id id "?"
  ;
  
syntax Constructor
  = Id name "(" {Arg ","}* args ")";
  
syntax JavaValue 
  = "null"
  | "true"
  | "false"
  | Int
  | {Enum"."}+ 
  ;
  
syntax Enum
  = Id \ JavaKeywords;
  
keyword JavaKeywords
  = "null" | "true" | "false";
  
syntax RascalValue 
  = "true"
  | "false"
  | Int
  | Id "(" {RascalValue ","}* ")"
  ;
  
  
lexical Int
  = [\-]?[0-9]+ !>> [0-9]
  ;


syntax Arg
  = Id name
  | Id type Id name "=" RascalValue value
  ;  


