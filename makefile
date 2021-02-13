run: Main
	./Main


Main: Main.hs
	ghc -package llvm-hs-pure -package llvm-hs -package mtl *.hs -outputdir build

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y
