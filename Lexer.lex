 structure Tokens = Tokens

 type pos = int

 type svalue = Tokens.svalue
 type ('a,'b) token = ('a,'b) Tokens.token
 type lexresult  = (svalue,pos) token

 val currentLine = ref 1
 fun eof () = Tokens.EOF(!currentLine, !currentLine)
 val lineStartPos = ref [0]



 fun keyword (s, pos) =
     case s of
         "D"             => Tokens.D (pos, pos)
       | "d"             => Tokens.D (pos, pos)
       | "Z"             => Tokens.Z (pos, pos)
       | "z"             => Tokens.Z (pos, pos)
       | "U"             => Tokens.CONC (pos, pos)
       | "sum"           => Tokens.SUM (pos, pos)
       | "sgn"           => Tokens.SIGN (pos, pos)
       | "mod"           => Tokens.MOD (pos, pos)
       | "least"         => Tokens.LEAST (pos, pos)
       | "largest"       => Tokens.LARGEST (pos, pos)
       | "count"         => Tokens.COUNT (pos, pos)
       | "drop"          => Tokens.DROP (pos, pos)
       | "keep"          => Tokens.KEEP (pos, pos)
       | "pick"          => Tokens.PICK (pos, pos)
       | "median"        => Tokens.MEDIAN (pos, pos)
       | "let"           => Tokens.LET (pos, pos)
       | "in"            => Tokens.IN (pos, pos)
       | "repeat"        => Tokens.REPEAT (pos, pos)
       | "accumulate"    => Tokens.ACCUM (pos, pos)
       | "while"         => Tokens.WHILE (pos, pos)
       | "until"         => Tokens.UNTIL (pos, pos)
       | "foreach"       => Tokens.FOREACH (pos, pos)
       | "do"            => Tokens.DO (pos, pos)
       | "if"            => Tokens.IF (pos, pos)
       | "then"          => Tokens.THEN (pos, pos)
       | "else"          => Tokens.ELSE (pos, pos)
       | "min"           => Tokens.MIN (pos, pos)
       | "max"           => Tokens.MAX (pos, pos)
       | "minimal"       => Tokens.MINIMAL (pos, pos)
       | "maximal"       => Tokens.MAXIMAL (pos, pos)
       | "choose"        => Tokens.CHOOSE (pos, pos)
       | "different"     => Tokens.DIFFERENT (pos, pos)
       | "function"      => Tokens.FUNCTION (pos, pos)
       | "call"          => Tokens.CALL (pos, pos)
       | "compositional" => Tokens.COMPOSITIONAL (pos, pos)
       | _               => Tokens.ID (s, pos, pos);


%%
%header (functor TrollLexFun(structure Tokens: troll_parser_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%
{ws}+                 => ( lex() );
[0-9]+                => ( Tokens.NUM (valOf (Int.fromString yytext), !currentLine, !currentLine) );
"0."[0-9]+            => ( Tokens.REAL (valOf (Real.fromString yytext),!currentLine, !currentLine));
"\\" [^ \n]*          => ( !currentLine; lex() );
"\""                  => ( lex() );
[a-zA-Z]+             => ( lex() );
"+"                   => ( lex() );
"-"                   => ( lex() );
"--"                  => ( lex() );
"*"                   => ( lex() );
"/"                   => ( lex() );
"("                   => ( lex() );
")"                   => ( lex() );
","                   => ( lex() );
";"                   => ( lex() );
"{"                   => ( lex() );
"}"                   => ( lex() );
":="                  => ( lex() );
"="                   => ( lex() );
"=/="                 => ( lex() );
"<"                   => ( lex() );
">"                   => ( lex() );
"<="                  => ( lex() );
">="                  => ( lex() );
".."                  => ( lex() );
"@"                   => ( lex() );
"&"                   => ( lex() );
"#"                   => ( lex() );
"?"                   => ( lex() );
"||"                  => ( lex() );
"|>"                  => ( lex() );
"<|"                  => ( lex() );
"<>"                  => ( lex() );
"'"                   => ( lex() );
"["                   => ( lex() );
"]"                   => ( lex() );
"%1"                  => ( lex() );
"%2"                  => ( lex() );
"~"                   => ( lex() );
"!"                   => ( lex() );
.                     => ( lex() );
