
import PARSER
import LEXER 
import System.Environment (getArgs) 
import Typing
import Evaluator
import Ast



main = do 
    args <- System.Environment.getArgs 
    --s <- getContents
    s <- readFile (head args)
    --d <- getLine
    putStrLn ""
    print "Scan Tokens are : "
    let a = scanTokens s
    print a
    putStrLn ""
    print "Parser Output is :"
    let b = parser a
    print b
    putStrLn ""
    print "Preorder Traversal of Parser Output is :"
    print (preordera b)
    putStrLn ""
    print "Type Checking :"
    let c=check b [] [] []
    if ((c==INTs)||(c==BOOLs)) 
    then print "Given Programme is Semantically Correct (Type Checking is OK) with resulting type as " 
    else print c 
    print c
    putStrLn ""
    print "Evaluating Programme (Final Result) :"
    let d = evalu b [] [] []
    if (c==BOOLs)  
    then print (reconvert d)
    else print d 

