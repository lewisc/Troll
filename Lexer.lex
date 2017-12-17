structure Tokens = Tokens

 type pos = int * int

 type svalue = Tokens.svalue
 type ('a,'b) token = ('a,'b) Tokens.token
 type lexresult  = (svalue,pos) token

 val currentLine = ref 1
 val lineStartPos = ref [0]



 fun keyword (s, pos) =
     case s of
         "D"            => Tokens.D pos
       | "d"            => Tokens.D pos
       | "Z"            => Tokens.Z pos
       | "z"            => Tokens.Z pos
       | "U"            => Tokens.CONC pos
       | "sum"          => Tokens.SUM pos
       | "sgn"          => Tokens.SIGN pos
       | "mod"          => Tokens.MOD pos
       | "least"        => Tokens.LEAST pos
       | "largest"      => Tokens.LARGEST pos
       | "count"        => Tokens.COUNT pos
       | "drop"         => Tokens.DROP pos
       | "keep"         => Tokens.KEEP pos
       | "pick"         => Tokens.PICK pos
       | "median"       => Tokens.MEDIAN pos
       | "let"          => Tokens.LET pos
       | "in"           => Tokens.IN pos
       | "repeat"       => Tokens.REPEAT pos
       | "accumulate"   => Tokens.ACCUM pos
       | "while"        => Tokens.WHILE pos
       | "until"        => Tokens.UNTIL pos
       | "foreach"      => Tokens.FOREACH pos
       | "do"           => Tokens.DO pos
       | "if"           => Tokens.IF pos
       | "then"         => Tokens.THEN pos
       | "else"         => Tokens.ELSE pos
       | "min"          => Tokens.MIN pos
       | "max"          => Tokens.MAX pos
       | "minimal"      => Tokens.MINIMAL pos
       | "maximal"      => Tokens.MAXIMAL pos
       | "choose"       => Tokens.CHOOSE pos
       | "different"    => Tokens.DIFFERENT pos
       | "function"     => Tokens.FUNCTION pos
       | "call"         => Tokens.CALL pos
       | "compositional"
	                => Tokens.COMPOSITIONAL pos
       | _              => Tokens.ID (s,pos);


%%
%header (functor TrollLexFun(structure Tokens: troll_parser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%
{ws}+                 => ( Token lexbuf );
[0-9]+                => ( case Int.fromString (getLexeme lexbuf) of
                           NONE   => lexerError lexbuf "Bad integer"
                         | SOME i => Tokens.NUM (i, getPos lexbuf));
"0."[0-9]+            => ( case Real.fromString (getLexeme lexbuf) of
                           NONE   => lexerError lexbuf "Bad number"
                         | SOME p => Tokens.REAL (p, getPos lexbuf));
"\\" [^ \n]*     => ( Token lexbuf );
"\""                  => ( Tokens.STRINGS (StringToken lexbuf, getPos lexbuf) );
[a-zA-Z]+            => ( keyword (getLexeme lexbuf, getPos lexbuf) );
"+"                   => ( Tokens.PLUS (getPos lexbuf) );
"-"                   => ( Tokens.MINUS (getPos lexbuf) );
"--"                  => ( Tokens.SETMINUS (getPos lexbuf) );
"*"                   => ( Tokens.TIMES (getPos lexbuf) );
"/"                   => ( Tokens.DIVIDE (getPos lexbuf) );
"("                   => ( Tokens.LPAR (getPos lexbuf) );
")"                   => ( Tokens.RPAR (getPos lexbuf) );
","                   => ( Parser.COMMA (getPos lexbuf) );
";"                   => ( Parser.SEMI (getPos lexbuf) );
"{"                   => ( Parser.LBRACE (getPos lexbuf) );
"}"                   => ( Parser.RBRACE (getPos lexbuf) );
":="                  => ( Parser.ASSGN (getPos lexbuf) );
"="                   => ( Parser.EQ (getPos lexbuf) );
"=/="                 => ( Parser.NEQ (getPos lexbuf) );
"<"                   => ( Parser.LT (getPos lexbuf) );
">"                   => ( Parser.GT (getPos lexbuf) );
"<="                  => ( Parser.LE (getPos lexbuf) );
">="                  => ( Parser.GE (getPos lexbuf) );
".."                  => ( Parser.DOTDOT (getPos lexbuf) );
"@"                   => ( Parser.CONC (getPos lexbuf) );
"&"                   => ( Parser.AND (getPos lexbuf) );
"#"                   => ( Parser.HASH (getPos lexbuf) );
"?"                   => ( Parser.QUESTION (getPos lexbuf) );
"||"                  => ( Parser.HCONC (getPos lexbuf) );
"|>"                  => ( Parser.VCONCL (getPos lexbuf) );
"<|"                  => ( Parser.VCONCR (getPos lexbuf) );
"<>"                  => ( Parser.VCONCC (getPos lexbuf) );
"'"                   => ( Parser.SAMPLE (getPos lexbuf) );
"["                   => ( Parser.LBRACK (getPos lexbuf) );
"]"                   => ( Parser.RBRACK (getPos lexbuf) );
"%1"                  => ( Parser.FIRST (getPos lexbuf) );
"%2"                  => ( Parser.SECOND (getPos lexbuf) );
"~"                   => ( Parser.TILDE (getPos lexbuf) );
"!"                   => ( Parser.BANG (getPos lexbuf) );
.                     => ( lexerError lexbuf "Illegal symbol in input" );
