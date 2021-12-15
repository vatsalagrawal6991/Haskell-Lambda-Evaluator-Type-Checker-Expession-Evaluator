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

