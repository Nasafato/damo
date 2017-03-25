/* OCamlYacc parser for Damo */

%{
open Ast
%}

%token PLUS MINS TIMES DIVIDE ASSIGN
%token <int> LITERAL
%token <string> ID
%token EOF