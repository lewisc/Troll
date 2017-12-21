all : Troll

Parser.grm.sml Parser.grm.desc Parser.grm.sig : Parser.grm
	mlyacc Parser.grm

Lexer.lex.sml : Lexer.lex
	mllex Lexer.lex

Troll : Troll.mlb Syntax.sml Parser.grm.sml Lexer.lex.sml Interpreter.sml Parser.grm.sml Rescope.sml Distribution.sml
	mlton Troll.mlb

clean :
	rm -rf Parser.grm.sml Parser.grm.sig Parser.grm.desc Lexer.lex.sml
