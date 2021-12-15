{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluator where

import Ast



type Env = []

convert True =1
convert False = 0
reconvert 0 =False
reconvert 1 =True

evalu a@(BinExpr Plus c d) env stk1 stk2 =          let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  y+x
evalu a@(BinExpr Minus c d) env stk1 stk2 =         let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  y-x
evalu a@(BinExpr Times c d) env stk1 stk2 =         let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  y*x
evalu a@(BinExpr Equals c d) env stk1 stk2 =        let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  convert(x==y)
evalu a@(BinExpr LessThan c d) env stk1 stk2 =      let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  convert(y<x)
evalu a@(BinExpr GreaterThan c d) env stk1 stk2 =   let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  convert(y>x)
evalu a@(BinExpr And c d) env stk1 stk2 =           let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  convert((reconvert y) && (reconvert x))
evalu a@(BinExpr Or c d) env stk1 stk2 =            let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in convert((reconvert y) || (reconvert x))
evalu a@(BinExpr Xor c d) env stk1 stk2 =           let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  if ((reconvert x)==True) && ((reconvert y)==False)
                                                        then 1 
                                                        else    if ((reconvert x)==False) && ((reconvert y)==True)
                                                                then 1 
                                                        else 0
evalu a@(BinExpr Implies c d) env stk1 stk2 =       let y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  if ((reconvert y)==True) && ((reconvert x)==False)
                                                        then 0
                                                        else 1
evalu a@(UnExpr Negate c) env stk1 stk2 =           let z=evalu c env stk1 stk2
                                                    in  -1*z
evalu a@(UnExpr Not c) env stk1 stk2 =              let z=evalu c env stk1 stk2
                                                    in  if z==0
                                                        then 1
                                                        else 0
evalu a@(Ite b c d) env stk1 stk2 =                 let z=evalu b env stk1 stk2
                                                        y=evalu c env stk1 stk2
                                                        x=evalu d env stk1 stk2
                                                    in  if z==1
                                                        then y
                                                        else x
evalu a@(NumExp b) env stk1 stk2 =                  b
evalu a@(BoolConst True) env stk1 stk2 =            1
evalu a@(BoolConst False) env stk1 stk2 =           0

evalu a@(VarExp b) env stk1 stk2 =                  let y= evaluenv b env stk1 stk2
                                                    in  y
evalu a@(Let (Decl b d) c) env stk1 stk2 =          let y= evalu d env stk1 stk2
                                                    in  evalu c ((b,y):env) stk1 stk2
evalu a@(Let (Func b d) c) env stk1 stk2 =          evalu c env ((b,d):stk1) stk2

evalu a@(AppExp b d) env stk1 stk2 =                let y =getfu b env stk1 stk2
                                                        z= evalu d env stk1 stk2
                                                    in  evaluf y z env stk1 stk2
        
evalu a@(AppExp' b d) env stk1 stk2 =               let z= evalu d env stk1 stk2
                                                    in  evaluf b z env stk1 stk2
getfu a env stk1@((z,y):x) stk2 =                   if z==a
                                                    then y
                                                    else evaluenv a x stk1 stk2
getfu a env stk1 stk2 = error ("not found "++show a ++show stk1)

evaluf a@(Fn b c d) e env stk1 stk2 =               let y= evalu d ((b,e):env) stk1 stk2
                                                    in  y
evaluf a@(Fun b c d e f) g env stk1 stk2 =          let y= evalu f ((c,g):env) ((b,a):stk1) stk2
                                                    in  y

evaluenv a env@((z,y):x) stk1 stk2 =                if z==a
                                                    then y
                                                    else evaluenv a x stk1 stk2
evaluenv a [] stk1 stk2 = error ("not found "++show a ++show stk1)
