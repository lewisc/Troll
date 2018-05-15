structure Tokens = Tokens

type pos = int

type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult  = (svalue,pos) token 
val currentLine = ref 1

fun eof () = Tokens.EOF(!currentLine, !currentLine)


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
%s S;
ws = [\ \t];

%%
<INITIAL>{ws}+                 => ( lex() );
<INITIAL>[0-9]+                => ( Tokens.NUM (valOf (Int.fromString yytext), !currentLine, !currentLine) );
<INITIAL>"0."[0-9]+            => ( Tokens.REAL (valOf (Real.fromString yytext),!currentLine, !currentLine) );
<INITIAL>"\\" [^ \n]*          => ( lex() );
<INITIAL>"\""                  => ( YYBEGIN S; lex() );
<INITIAL>[a-zA-Z]+             => ( keyword (yytext, !currentLine) );
<INITIAL>"+"                   => ( Tokens.PLUS (!currentLine, !currentLine) );
<INITIAL>"-"                   => ( Tokens.MINUS (!currentLine, !currentLine) );
<INITIAL>"--"                  => ( Tokens.SETMINUS (!currentLine, !currentLine) );
<INITIAL>"*"                   => ( Tokens.TIMES (!currentLine, !currentLine) );
<INITIAL>"/"                   => ( Tokens.DIVIDE (!currentLine, !currentLine) );
<INITIAL>"("                   => ( Tokens.LPAR(!currentLine, !currentLine) );
<INITIAL>")"                   => ( Tokens.RPAR(!currentLine, !currentLine) );
<INITIAL>","                   => ( Tokens.COMMA (!currentLine, !currentLine) );
<INITIAL>";"                   => ( Tokens.SEMI (!currentLine, !currentLine) );
<INITIAL>"{"                   => ( Tokens.LBRACE (!currentLine, !currentLine) );
<INITIAL>"}"                   => ( Tokens.RBRACE (!currentLine, !currentLine) );
<INITIAL>":="                  => ( Tokens.ASSGN (!currentLine, !currentLine) );
<INITIAL>"="                   => ( Tokens.EQ (!currentLine, !currentLine) );
<INITIAL>"=/="                 => ( Tokens.NEQ (!currentLine, !currentLine) );
<INITIAL>"<"                   => ( Tokens.LT (!currentLine, !currentLine) );
<INITIAL>">"                   => ( Tokens.GT (!currentLine, !currentLine) );
<INITIAL>"<="                  => ( Tokens.LE (!currentLine, !currentLine) );
<INITIAL>">="                  => ( Tokens.GE (!currentLine, !currentLine) );
<INITIAL>".."                  => ( Tokens.DOTDOT (!currentLine, !currentLine) );
<INITIAL>"@"                   => ( Tokens.CONC (!currentLine, !currentLine) );
<INITIAL>"&"                   => ( Tokens.AND (!currentLine, !currentLine) );
<INITIAL>"#"                   => ( Tokens.HASH (!currentLine, !currentLine) );
<INITIAL>"?"                   => ( Tokens.QUESTION (!currentLine, !currentLine) );
<INITIAL>"||"                  => ( Tokens.HCONC (!currentLine, !currentLine) );
<INITIAL>"|>"                  => ( Tokens.VCONCL (!currentLine, !currentLine) );
<INITIAL>"<|"                  => ( Tokens.VCONCR (!currentLine, !currentLine) );
<INITIAL>"<>"                  => ( Tokens.VCONCC (!currentLine, !currentLine) );
<INITIAL>"'"                   => ( Tokens.SAMPLE (!currentLine, !currentLine) );
<INITIAL>"["                   => ( Tokens.LBRACK (!currentLine, !currentLine) );
<INITIAL>"]"                   => ( Tokens.RBRACK (!currentLine, !currentLine) );
<INITIAL>"%1"                  => ( Tokens.FIRST (!currentLine, !currentLine) );
<INITIAL>"%2"                  => ( Tokens.SECOND (!currentLine, !currentLine) );
<INITIAL>"~"                   => ( Tokens.TILDE (!currentLine, !currentLine) );
<INITIAL>"!"                   => ( Tokens.BANG (!currentLine, !currentLine) );
<S>[^\"]*                      => ( Tokens.STRINGS ( [yytext], !currentLine, !currentLine) );
<S>"\""                        => ( YYBEGIN INITIAL; lex() );
