# Made By Vatsal Agrawal
# Haskell-Lambda-Evaluator-Type-Checker-Expession-Evaluator

Haskell Type Language parsing, token generation, type checker, expression evaluator, lambda expression evaluator with parse tree and ast generation and error raising

**************************HOW TO MAKE EXECUTABLE AND RUN PROGRAMME******************************
For Running Programme :-
	1) Run make command on terminal
	or 
	1)alex lexer.x -o LEXER.hs
	2)happy grammar.y -o PARSER.hs
	3)ghc --make MAIN.hs -o a3
	
For Testing :-
	Can use any of the commands
	1) make fibo (for running given input file)
	2) make fact (for running given input file)
	3) make run (for running correct programme syntax)
	4) make valid1 (for running correct programme syntax)
	5) make valid2 (for running correct programme syntax)
	6) make invalid1 (for showing that programme detect error)
	7) make invalid2 (for showing that programme detect error)
	8) make invalid3 (for showing that programme detect error)
	7) make invalid4 (for showing that programme detect error)
	
Note :-
	I have used skeletals programme provided with my assignment and have not used my own programme of a2 because
	1) A lot of modification would be needed in that to add function grammar
	2) It is lot easier using happy to construct type checker
	
	Also, the skeletal provided with a3 do not inherently support multiple declarartion in let and it needs given EOF to end statment
My original programme of a2 do not have this constraint (is better) so it needs more modification
I have assumed that application of function is expression 
and Every programme is a expression 
we cannot create programme with a named or anonymous function until we have application of it 
But we can create assignment of a function
named Function type also support curry notation
AppExp is used to donate application

Output Contains
	1) Scanned tokens list
	2) Abstract Syntax tree
	3) Preorder of AST
	4) Type Checking
	5) Final Evaluation Result
	
	
Problem Statment 
support for the following concrete syntax should be provided:
• Anonymous function: fn (x :: typ) ⇒ exp end
• Named function: fun hnamei(y :: typ) :: typ ⇒ exp end
• Application: (f A)
It is worth noticing that one cannot define a recursive function through an anonymous function.
For this assignment, recursive functions can be implemented only through named functions.

implement a type checker for this extended language.
Each expression is either well-typed (i.e., associated with at most one type) or an exception
must be raised. For instance, if e1 then e2 else e3 fi expression is well-typed when e1
is a boolean type and e2 and e3 must have the same types. In order to implement the type
checker, you must implement a type grammar (that provides you with rules that must be
searched and a derivation has to be obtained to conclude the type checking).

if the type checking goes through successfully, then perform the evaulation of an input
program for the extended language using an evaluation strategy call-by-value. Note that for
evaluation of an application, one will have to consider computing evironment closure (capturing
the enclosing environment).

The parser output should be  pre-order traversal of
the AST.
3. The type-checker output should throw appropriate exceptions, if any. For instance, when
looking up a variable x in the environment, if no type is assigned then the typechecker should
throw an error along the line: Var x w/o a type. Consider another example of function
application (f 3). If the function argument for f doesn’t match the int type of 3, then the
typechecker should throw an error along the lines: Application argument mismatch in f.
