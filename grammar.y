{
module PARSER where
import Ast
import LEXER
}

%name parser
%tokentype { Token }
%error { parseError }

%token
       ";"     { EOF $$ }
       "("     { LPAREN $$ }
       ")"     { RPAREN $$ }
       -- int ops
       "+"     { PLUS $$ }
       "-"     { MINUS $$ }
       "*"     { TIMES $$ }
       "~"     { NEGATE $$ }
       "="     { EQUALS $$ }
       "<"     { LESSTHAN $$ }
       ">"     { GREATERTHAN $$ }
       -- bool ops
       "!"     { NOT $$ }
       "&&"    { AND $$ }
       "||"    { OR $$ }
       "^"     { XOR $$ }
       --"=>"    { IMPLIES $$ }
       --function 
       "fn"    { FN $$ }
       "fun"   { FUN $$ }
       "ints"   { INT $$}
       "bools"  { BOOL $$}
       "->"    { ARROW $$}
       "=>"    { INFERENCE $$}
       "::"    { TYPE $$}
       "done"  { DONE $$}
       "if"    { IF $$ }
       "then"  { THEN $$ }
       "else"  { ELSE $$ }
       "fi"    { FI $$ }
       -- let
       "let"   { LET $$ }
       ":="    { ASSIGN $$ }
       "in"    { IN $$ }
       "end"   { END $$ }
       -- atoms
       "false" { FALSE $$ }
       "true"  { TRUE $$ }
       int     { CONST $$ }
       var     { ID $$ }

%nonassoc ">" "<" "=" 
%left "+" "-"
%left "*" 
%left "^" "||" "&&"
%right "!" "~" "->"

%%

Start
    : Expr ";"				                 { $1 }

Typea 
    : "ints"                                  { INTs}
    | "bools"                                 { BOOLs}
    | Typea "->" Typea                     { Arrow $1 $3 }                         

Decl
    : var ":=" Expr                          { Decl $1 $3 }
    | var ":=" Funct                         { Func $1 $3 }

Funct
    : "fn" "(" var "::" Typea ")" "=>" Expr "done"                       { Fn $3 $5 $8} 
    | "fun" var "(" var "::" Typea ")" "::" Typea "=>" Expr "done"      { Fun $2 $4 $6 $9 $11} 

App 
    : "(" Funct Expr ")"                     {AppExp' $2 $3}
    | "(" var Expr ")"                       {AppExp $2 $3}

Expr
    : App                                    { $1 }
    | "(" Expr ")"                           { $2 }

    -- int ops
    | "~" Expr                               { UnExpr Negate $2 }
    | Expr "+" Expr                          { BinExpr Plus $1 $3 }
    | Expr "-" Expr                          { BinExpr Minus $1 $3 }
    | Expr "*" Expr                          { BinExpr Times $1 $3 }
    | Expr "=" Expr                          { BinExpr Equals $1 $3 }
    | Expr "<" Expr                          { BinExpr LessThan $1 $3 }
    | Expr ">" Expr                          { BinExpr GreaterThan $1 $3 }

    -- bool ops
    | "!" Expr                               { UnExpr Not $2 }
    | Expr "&&" Expr                         { BinExpr And $1 $3 }
    | Expr "||" Expr                         { BinExpr Or $1 $3 }
    | Expr "^" Expr                          { BinExpr Xor $1 $3 }
    -- | Expr "=>" Expr                         { BinExpr Implies $1 $3 }

    -- specials
    | "let" Decl "in" Expr "end"             { Let $2 $4 }
    | "if" Expr "then" Expr "else" Expr "fi" { Ite $2 $4 $6 }

    -- atom
    | "false"                                { BoolConst False }
    | "true"                                 { BoolConst True }
    | int                                    { NumExp $1 }
    | var                                    { VarExp $1 }
    

{

parseError [] = error $ "Syntax Error in the last token"
parseError (x:xs) = error $ "Syntax Error: " ++ (show x)

}
