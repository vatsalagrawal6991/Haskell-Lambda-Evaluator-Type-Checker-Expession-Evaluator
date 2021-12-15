all:
	alex lexer.x -o LEXER.hs
	happy grammar.y -o PARSER.hs
	ghc --make MAIN.hs -o a3
fibo : all
	./a3 ./INPUT/fibo.txt
fact : all
	./a3 ./INPUT/fact.txt
run : all
	./a3 ./INPUT/input.txt
valid1 : all
	./a3 ./INPUT/input_2.txt
valid2 : all
	./a3 ./INPUT/input_4.txt
invalid1 : all
	./a3 ./INPUT/invalid_1.txt
invalid2 : all
	./a3 ./INPUT/invalid_2.txt
invalid3 : all
	./a3 ./INPUT/invalid_3.txt
invalid4 : all
	./a3 ./INPUT/invalid_4.txt

