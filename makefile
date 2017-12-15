all : Parser.grm.sml Lexer.lex.sml

Parser.grm.sml : Parser.grm
	mlyacc Parser.grm

Lexer.lex.sml : Lexer.lex
	mllex Lexer.lex

clean :
	rm -rf Parser.grm.sml Parser.grm.sig Parser.grm.desc
