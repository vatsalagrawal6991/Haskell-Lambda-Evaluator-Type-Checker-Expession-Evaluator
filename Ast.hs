module Ast where

        type Id = String

        data Types = INTs | BOOLs | Arrow Types Types
                deriving (Eq,Show)

        data VarDecl = Decl Id Expr | Func Id Function
                deriving (Eq,Show)

        data UnOp = Negate | Not
                deriving (Eq,Show)

        data BinOp = Plus | Minus | Times | Equals | LessThan | GreaterThan | And | Or | Xor | Implies
                deriving (Eq,Show)

        data Expr =
                     BinExpr BinOp Expr Expr
                   | UnExpr UnOp Expr
                   | Ite Expr Expr Expr
                   | Let VarDecl Expr
                   | BoolConst Bool
                   | NumExp Integer
                   | VarExp Id
                   | AppExp Id Expr
                   | AppExp' Function Expr
                   deriving (Eq,Show)

        data Function =   
                          Fn Id Types Expr 
                        | Fun Id Id Types Types Expr
                        deriving (Eq, Show)

        preordera (BinExpr Plus b c) = ["BinExpr Plus"]++(preordera b)++(preordera c) 
        preordera (BinExpr Minus b c) = ["BinExpr Minus"]++(preordera b)++(preordera c)
        preordera (BinExpr Times b c) = ["BinExpr Times"]++(preordera b)++(preordera c) 
        preordera (BinExpr Equals b c) = ["BinExpr Equals"]++(preordera b)++(preordera c)
        preordera (BinExpr LessThan b c) = ["BinExpr LessThan"]++(preordera b)++(preordera c)
        preordera (BinExpr GreaterThan b c) = ["BinExpr GreaterThan"]++(preordera b)++(preordera c)
        preordera (BinExpr And b c) = ["BinExpr And"]++(preordera b)++(preordera c)
        preordera (BinExpr Or b c) = ["BinExpr Or"]++(preordera b)++(preordera c)
        preordera (BinExpr Xor b c) = ["BinExpr Xor"]++(preordera b)++(preordera c)
        preordera (BinExpr Implies b c) = ["BinExpr Implies"]++(preordera b)++(preordera c)
        preordera (UnExpr Not a) = ["UnExpr Not"]++(preordera a)
        preordera (UnExpr Negate a) = ["UnExpr Negate"]++(preordera a)
        preordera (Ite a b c) = ["Ite"]++(preordera a)++(preordera b)++(preordera c)
        preordera (Let (Decl a b) c) = ["Let Decl"]++(preorderi a)++(preordera b)++(preordera c)
        preordera (Let (Func a b) c) = ["Let Decl"]++(preorderi a)++(preorderf b)++(preordera c)
        preordera (BoolConst True) = ["BoolConst True"]
        preordera (BoolConst False) = ["BoolConst False"]
        preordera (NumExp a) = ["NumExp "++(show a)]
        preordera (VarExp a) = ["VarExp " ++ a]
        preordera (AppExp a b) = ["AppExp"]++(preorderi a)++(preordera b)
        preordera (AppExp' a b) = ["AppExp'"]++(preorderf a)++(preordera b)
        



        preorderi a = [a] 
        preorderf (Fn a b c)= ["Fn"]++(preorderi a)++(preordert b)++(preordera c)
        preorderf (Fun a b c d e)= ["Fn"]++(preorderi a)++(preorderi b)++(preordert c)++(preordert d)++(preordera e)   
        preordert INTs = ["INTs"]
        preordert BOOLs = ["BOOLs"]
        preordert (Arrow a b) = ["Arrow"]++(preordert a)++(preordert b)