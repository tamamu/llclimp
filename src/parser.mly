%{ open Ast %}

%token Nil
%token T
%token <string> Symbol
%token <int> Integer
%token <float> Float
%token <string> String
%token LParen RParen Eof

%start main
%type <Ast.statement list> main

%start main_statement
%type <Ast.statement option> main_statement

%%

main:
| statements = list(statement) Eof {statements}

main_statement:
| statement = statement {Some statement}
| Eof {None}

statement:
| sexp=sexp {Sexp sexp}
| atom=atom {Atom atom}

sexp:
| LParen statements=list(statement) RParen {statements}


atom:
| Nil {Nil}
| T {T}
| i=Integer {Integer i}
| f=Float {Float f}
| str=String {String str}
| sym=Symbol {Symbol sym}
