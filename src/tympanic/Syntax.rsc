@license{Copyright (c) 2018, Rodin Aarssen, Centrum Wiskunde & Informatica (CWI) 
All rights reserved. 
 
Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 
 
1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
  
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

}
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
  | "(" Id castType ")" Id id
  | "(" Id castType "[" "]" ")" Id id
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


