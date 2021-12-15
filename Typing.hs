

module Typing where

import Ast

type Env = [(Id,Types)]

check a@(BinExpr Plus c d) env stk1 stk2 =          let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (x== INTs)&& (y== INTs)
                                                        then INTs
                                                        else error $ "Plus Operation - type mismatch" 
check a@(BinExpr Minus c d) env stk1 stk2 =         let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (x== INTs)&& (y== INTs)
                                                        then INTs
                                                        else error $ "Minus Operation - type mismatch" 
check a@(BinExpr Times c d) env stk1 stk2 =         let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (x== INTs)&& (y== INTs)
                                                        then INTs
                                                        else error $ "Times Operation - type mismatch"
check a@(BinExpr Equals c d) env stk1 stk2 =        let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if x==y
                                                        then BOOLs
                                                        else error $ "Equals Operation - type mismatch"
check a@(BinExpr LessThan c d) env stk1 stk2 =      let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (x== INTs)&& (y== INTs)
                                                        then BOOLs
                                                        else error $ "LessThan Operation - type mismatch"
check a@(BinExpr GreaterThan c d) env stk1 stk2 =   let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (x== INTs)&& (y== INTs)
                                                        then BOOLs
                                                        else error $ "GreaterThan Operation - type mismatch"
check a@(BinExpr And c d) env stk1 stk2 =           let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (x== BOOLs )&& (y== BOOLs )
                                                        then BOOLs
                                                        else error $ "And Operation - type mismatch"
check a@(BinExpr Or c d) env stk1 stk2 =            let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in if (y== BOOLs )&& (x== BOOLs )
                                                        then BOOLs
                                                        else error $ "Or Operation - type mismatch"
check a@(BinExpr Xor c d) env stk1 stk2 =           let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (y== BOOLs )&& (x== BOOLs )
                                                        then BOOLs
                                                        else error $ "Xor Operation - type mismatch"
check a@(BinExpr Implies c d) env stk1 stk2 =       let y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (y== BOOLs )&& (x== BOOLs )
                                                        then BOOLs
                                                        else error $ "Implies Operation - type mismatch"
check a@(UnExpr Negate c) env stk1 stk2 =           let z=check c env stk1 stk2
                                                    in  if z== INTs
                                                        then INTs
                                                        else error $ "Negate Operation - type mismatch"
check a@(UnExpr Not c) env stk1 stk2 =              let z=check c env stk1 stk2
                                                    in  if z== BOOLs
                                                        then BOOLs
                                                        else error $ "Not Operation - type mismatch"
check a@(Ite b c d) env stk1 stk2 =                 let z=check b env stk1 stk2
                                                        y=check c env stk1 stk2
                                                        x=check d env stk1 stk2
                                                    in  if (z== BOOLs) && (y==x)
                                                        then y
                                                        else error $ "ite Operation - type mismatch"
check a@(NumExp b) env stk1 stk2 =                  INTs
check a@(BoolConst b) env stk1 stk2 =               BOOLs
check a@(VarExp b) env stk1 stk2 =                  let y= checkenv b env stk1 stk2
                                                    in  y
check a@(Let (Decl b d) c) env stk1 stk2 =          let y= check d env stk1 stk2
                                                    in  check c ((b,y):env) stk1 stk2

check a@(Let (Func b d) c) env stk1 stk2 =          let (y,z)= checkf d env stk1 stk2
                                                    in  check c ((b,y):env) ((b,z):stk1) stk2

check a@(AppExp b d) env stk1 stk2 =                let z=check d env stk1 stk2
                                                        y= checkis b z env stk1 stk2
                                                    in  y
check a@(AppExp' b d) env stk1 stk2 =               let z=check d env stk1 stk2
                                                        (x,w)=checkf b env stk1 stk2
                                                    in  if z==w
                                                        then x
                                                        else error $ "Application arguments - type mismatch - "++show a

checkf a@(Fn b c d) env stk1 stk2 =                 let y= check d ((b,c):env) stk1 stk2
                                                    in  (y,c)
checkf a@(Fun b c d e f) env stk1 stk2 =            let (x,z)=getright e
                                                        y= check f ((b,z):(c,d):env) ((b,z):stk1) (x:stk2)
                                                    in  if y==z
                                                        then (y,d)
                                                        else error $ "Application result type mismatch - "++show a

checkis a b env stk1@((z,y):x) stk2 =             if (z==a) && (y==b)
                                                  then checkenv a env stk1 stk2
                                                  else checkis a b env x stk2
checkis a b env [] stk2 = error $ "Application Variable - type mismatch - "++show a

checkenv a env@((z,y):x) stk1 stk2 =                if z==a
                                                    then y
                                                    else checkenv a x stk1 stk2
checkenv a [] stk1 stk2 = error $ "Variable w/o a type (not found) - "++show a

getright INTs = ([],INTs) 
getright BOOLs = ([],BOOLs) 
getright (Arrow a b) =  let (y,z)=getright b 
                        in (a:y,z)
    