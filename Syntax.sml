structure Syntax =
struct

  type pos = int * int       (* position in program (line,column) *)

  datatype Exp = NUM of int * pos
               | ID of string * pos
               | EMPTY
               | CONC of Exp * Exp * pos
               | FROMTO of Exp * Exp * pos
               | CHOOSE of Exp * pos
               | DIFFERENT of Exp * pos
               | PLUS of Exp * Exp * pos
               | MINUS of Exp * Exp * pos
               | TIMES of Exp * Exp * pos
               | DIVIDE of Exp * Exp * pos
               | MOD of Exp * Exp * pos
               | UMINUS of Exp * pos
               | SIGN of Exp * pos
               | D of Exp * pos
               | Z of Exp * pos
               | SUM of Exp * pos
               | COUNT of Exp * pos
               | LEAST of Exp * Exp * pos
               | LARGEST of Exp * Exp * pos
               | MINIMAL of Exp * pos
               | MAXIMAL of Exp * pos
               | HASH of Exp * Exp * pos
               | AND of Exp * Exp * pos
               | EQ of Exp * Exp * pos
               | NEQ of Exp * Exp * pos
               | LT of Exp * Exp * pos
               | GT of Exp * Exp * pos
               | LE of Exp * Exp * pos
               | GE of Exp * Exp * pos
               | DROP of Exp * Exp * pos
               | KEEP of Exp * Exp * pos
               | PICK of Exp * Exp * pos
               | SETMINUS of Exp * Exp * pos
               | MEDIAN of Exp * pos
               | LET of string * Exp * Exp * pos
               | REPEAT of string * Exp * Exp * bool * pos
               | ACCUM of string * Exp * Exp * bool * pos
	       | FOREACH of string * Exp * Exp * pos
	       | IF of Exp * Exp * Exp * pos
	       | CALL of string * Exp list * pos
	       | STRING of string * pos
               | SAMPLE of Exp * pos
               | SAMPLES of Exp * Exp * pos
               | HCONC of Exp * Exp * pos
               | VCONCL of Exp * Exp * pos
               | VCONCR of Exp * Exp * pos
               | VCONCC of Exp * Exp * pos
	       | QUESTION of real * pos
               | PAIR of Exp * Exp * pos
               | FIRST of Exp * pos
               | SECOND of Exp * pos
               | DEFAULT of string * Exp * pos


  (* (name, (args, body, position)) *)
  datatype Declaration = Func of (string list * Exp * pos)
                       | Comp of (Exp * string * string * pos)

  type Program = (string * Declaration) list * Exp

  fun showExp exp =
    case exp of
      NUM (i,p) => Int.toString i
    | EMPTY => "{}"
    | ID (x,p) => x
    | CONC (e1, e2, p) => "{" ^ showExp e1 ^ ", " ^ showExp e2 ^ "}"
    | FROMTO (e1, e2, p) => "(" ^ showExp e1 ^ ".." ^ showExp e2 ^ ")"
    | CHOOSE (e1,p) => "(choose " ^ showExp e1 ^ ")"
    | DIFFERENT (e1,p) => "(different " ^ showExp e1 ^ ")"
    | PLUS (e1, e2, p) => "(" ^ showExp e1 ^ "+" ^ showExp e2 ^ ")"
    | MINUS (e1, e2, p) => "(" ^ showExp e1 ^ "-" ^ showExp e2 ^ ")"
    | TIMES (e1, e2, p) => "(" ^ showExp e1 ^ "*" ^ showExp e2 ^ ")"
    | DIVIDE (e1, e2, p) => "(" ^ showExp e1 ^ "/" ^ showExp e2 ^ ")"
    | MOD (e1, e2, p) => "(" ^ showExp e1 ^ " mod " ^ showExp e2 ^ ")"
    | UMINUS (e1,p) => "(- " ^ showExp e1 ^ ")"
    | D (e1,p) => "(D " ^ showExp e1 ^ ")"
    | Z (e1,p) => "(Z " ^ showExp e1 ^ ")"
    | SIGN (e1,p) => "(sgn " ^ showExp e1 ^ ")"
    | SUM (e1,p) => "(sum " ^ showExp e1 ^ ")"
    | COUNT (e1,p) => "(count " ^ showExp e1 ^ ")"
    | LEAST (e1, e2, p) => "(least " ^ showExp e1 ^ " " ^ showExp e2 ^ ")"
    | LARGEST (e1, e2, p) => "(largest " ^ showExp e1 ^ " " ^ showExp e2 ^ ")"
    | MINIMAL (e1,p) => "(min " ^ showExp e1 ^ ")"
    | MAXIMAL (e1,p) => "(max " ^ showExp e1 ^ ")"
    | HASH (e1, e2, p) => "(" ^ showExp e1 ^ " # " ^ showExp e2 ^ ")"
    | AND (e1, e2, p) => "(" ^ showExp e1 ^ " & " ^ showExp e2 ^ ")"
    | EQ (e1, e2, p) => "(" ^ showExp e1 ^ " = " ^ showExp e2 ^ ")"
    | NEQ (e1, e2, p) => "(" ^ showExp e1 ^ " =/= " ^ showExp e2 ^ ")"
    | LT (e1, e2, p) => "(" ^ showExp e1 ^ " < " ^ showExp e2 ^ ")"
    | GT (e1, e2, p) => "(" ^ showExp e1 ^ " > " ^ showExp e2 ^ ")"
    | LE (e1, e2, p) => "(" ^ showExp e1 ^ " <= " ^ showExp e2 ^ ")"
    | GE (e1, e2, p) => "(" ^ showExp e1 ^ " >= " ^ showExp e2 ^ ")"
    | DROP (e1, e2, p) => "(" ^ showExp e1 ^ " drop " ^ showExp e2 ^ ")"
    | KEEP (e1, e2, p) => "(" ^ showExp e1 ^ " keep " ^ showExp e2 ^ ")"
    | PICK (e1, e2, p) => "(" ^ showExp e1 ^ " pick " ^ showExp e2 ^ ")"
    | SETMINUS (e1, e2, p) => "(" ^ showExp e1 ^ " -- " ^ showExp e2 ^ ")"
    | MEDIAN (e1,p) => "(median " ^ showExp e1 ^ ")"
    | LET (x, e1, e2, p) =>
        "(" ^ x ^ " := " ^ showExp e1 ^ ";\n" ^ showExp e2 ^ ")"
    | REPEAT (x, e1, e2, b, p) =>
        "(repeat " ^ x ^ " := " ^ showExp e1 ^
        (if b then " while " else " until ") ^ showExp e2 ^ ")"
    | ACCUM (x, e1, e2, b, p) =>
        "(accumulate " ^ x ^ " := " ^ showExp e1 ^
        (if b then " while " else " until ") ^ showExp e2 ^ ")"
    | FOREACH (x, e1, e2, p) =>
        "(foreach " ^ x ^ " in" ^ showExp e1 ^ " do " ^ showExp e2 ^ ")"
    | IF (e1, e2, e3, p) =>
        "(if " ^ showExp e1 ^ "\nthen " ^ showExp e2 ^ "\nelse " ^ showExp e3 ^ ")"
    | CALL (f, es, p) =>
        "call " ^ f ^ "(" ^
        String.concat (List.map (fn e => showExp e ^ ",") es) ^ ")"
    | STRING (s, p) => "\"" ^ s ^ "\""
    | SAMPLE (e1,p) => "'( " ^ showExp e1 ^ ")"
    | SAMPLES (e1, e2, p) => "(" ^ showExp e1 ^ " ' " ^ showExp e2 ^ ")"
    | HCONC (e1, e2, p) => "(" ^ showExp e1 ^ " || " ^ showExp e2 ^ ")"
    | VCONCL (e1, e2, p) => "(" ^ showExp e1 ^ " |> " ^ showExp e2 ^ ")"
    | VCONCR (e1, e2, p) => "(" ^ showExp e1 ^ " <| " ^ showExp e2 ^ ")"
    | VCONCC (e1, e2, p) => "(" ^ showExp e1 ^ " <> " ^ showExp e2 ^ ")"
    | QUESTION (q,p) => "?" ^ Real.toString q
    | PAIR (e1, e2, p) => "[" ^ showExp e1 ^ " , " ^ showExp e2 ^ "]"
    | FIRST (e1,p) => "%1( " ^ showExp e1 ^ ")"
    | SECOND (e1,p) => "%2( " ^ showExp e1 ^ ")"
    | DEFAULT (x,e1,p) => "(" ^ x ^" ~ " ^ showExp e1 ^ ")"
end
