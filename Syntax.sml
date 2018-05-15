structure Syntax =
struct

  type pos = int * int       (* position in program (line,column) *)

  datatype Exp = NUM of int
               | ID of string
               | EMPTY
               | CONC of Exp * Exp
               | FROMTO of Exp * Exp
               | CHOOSE of Exp
               | DIFFERENT of Exp
               | PLUS of Exp * Exp
               | MINUS of Exp * Exp
               | TIMES of Exp * Exp
               | DIVIDE of Exp * Exp
               | MOD of Exp * Exp
               | UMINUS of Exp
               | SIGN of Exp
               | D of Exp
               | Z of Exp
               | SUM of Exp
               | COUNT of Exp
               | LEAST of Exp * Exp
               | LARGEST of Exp * Exp
               | MINIMAL of Exp
               | MAXIMAL of Exp
               | HASH of Exp * Exp
               | AND of Exp * Exp
               | EQ of Exp * Exp
               | NEQ of Exp * Exp
               | LT of Exp * Exp
               | GT of Exp * Exp
               | LE of Exp * Exp
               | GE of Exp * Exp
               | DROP of Exp * Exp
               | KEEP of Exp * Exp
               | PICK of Exp * Exp
               | SETMINUS of Exp * Exp
               | MEDIAN of Exp
               | LET of string * Exp * Exp
               | REPEAT of string * Exp * Exp * bool
               | ACCUM of string * Exp * Exp * bool
               | FOREACH of string * Exp * Exp
               | IF of Exp * Exp * Exp
               | CALL of string * Exp list
               | STRING of string
               | SAMPLE of Exp
               | SAMPLES of Exp * Exp
               | HCONC of Exp * Exp
               | VCONCL of Exp * Exp
               | VCONCR of Exp * Exp
               | VCONCC of Exp * Exp
               | QUESTION of real
               | PAIR of Exp * Exp
               | FIRST of Exp
               | SECOND of Exp
               | DEFAULT of string * Exp


  (* (name, (args, body, position)) *)
  datatype Declaration = Func of (string list * Exp)
                       | Comp of (Exp * string * string)

  type Program = (string * Declaration) list * Exp

  fun showExp exp =
    case exp of
      NUM (i) => Int.toString i
    | EMPTY => "{}"
    | ID (x) => x
    | CONC (e1, e2) => "{" ^ showExp e1 ^ ", " ^ showExp e2 ^ "}"
    | FROMTO (e1, e2) => "(" ^ showExp e1 ^ ".." ^ showExp e2 ^ ")"
    | CHOOSE (e1) => "(choose " ^ showExp e1 ^ ")"
    | DIFFERENT (e1) => "(different " ^ showExp e1 ^ ")"
    | PLUS (e1, e2) => "(" ^ showExp e1 ^ "+" ^ showExp e2 ^ ")"
    | MINUS (e1, e2) => "(" ^ showExp e1 ^ "-" ^ showExp e2 ^ ")"
    | TIMES (e1, e2) => "(" ^ showExp e1 ^ "*" ^ showExp e2 ^ ")"
    | DIVIDE (e1, e2) => "(" ^ showExp e1 ^ "/" ^ showExp e2 ^ ")"
    | MOD (e1, e2) => "(" ^ showExp e1 ^ " mod " ^ showExp e2 ^ ")"
    | UMINUS (e1) => "(- " ^ showExp e1 ^ ")"
    | D (e1) => "(D " ^ showExp e1 ^ ")"
    | Z (e1) => "(Z " ^ showExp e1 ^ ")"
    | SIGN (e1) => "(sgn " ^ showExp e1 ^ ")"
    | SUM (e1) => "(sum " ^ showExp e1 ^ ")"
    | COUNT (e1) => "(count " ^ showExp e1 ^ ")"
    | LEAST (e1, e2) => "(least " ^ showExp e1 ^ " " ^ showExp e2 ^ ")"
    | LARGEST (e1, e2) => "(largest " ^ showExp e1 ^ " " ^ showExp e2 ^ ")"
    | MINIMAL (e1) => "(min " ^ showExp e1 ^ ")"
    | MAXIMAL (e1) => "(max " ^ showExp e1 ^ ")"
    | HASH (e1, e2) => "(" ^ showExp e1 ^ " # " ^ showExp e2 ^ ")"
    | AND (e1, e2) => "(" ^ showExp e1 ^ " & " ^ showExp e2 ^ ")"
    | EQ (e1, e2) => "(" ^ showExp e1 ^ " = " ^ showExp e2 ^ ")"
    | NEQ (e1, e2) => "(" ^ showExp e1 ^ " =/= " ^ showExp e2 ^ ")"
    | LT (e1, e2) => "(" ^ showExp e1 ^ " < " ^ showExp e2 ^ ")"
    | GT (e1, e2) => "(" ^ showExp e1 ^ " > " ^ showExp e2 ^ ")"
    | LE (e1, e2) => "(" ^ showExp e1 ^ " <= " ^ showExp e2 ^ ")"
    | GE (e1, e2) => "(" ^ showExp e1 ^ " >= " ^ showExp e2 ^ ")"
    | DROP (e1, e2) => "(" ^ showExp e1 ^ " drop " ^ showExp e2 ^ ")"
    | KEEP (e1, e2) => "(" ^ showExp e1 ^ " keep " ^ showExp e2 ^ ")"
    | PICK (e1, e2) => "(" ^ showExp e1 ^ " pick " ^ showExp e2 ^ ")"
    | SETMINUS (e1, e2) => "(" ^ showExp e1 ^ " -- " ^ showExp e2 ^ ")"
    | MEDIAN (e1) => "(median " ^ showExp e1 ^ ")"
    | LET (x, e1, e2) =>
        "(" ^ x ^ " := " ^ showExp e1 ^ ";\n" ^ showExp e2 ^ ")"
    | REPEAT (x, e1, e2, b) =>
        "(repeat " ^ x ^ " := " ^ showExp e1 ^
        (if b then " while " else " until ") ^ showExp e2 ^ ")"
    | ACCUM (x, e1, e2, b) =>
        "(accumulate " ^ x ^ " := " ^ showExp e1 ^
        (if b then " while " else " until ") ^ showExp e2 ^ ")"
    | FOREACH (x, e1, e2) =>
        "(foreach " ^ x ^ " in" ^ showExp e1 ^ " do " ^ showExp e2 ^ ")"
    | IF (e1, e2, e3) =>
        "(if " ^ showExp e1 ^ "\nthen " ^ showExp e2 ^ "\nelse " ^ showExp e3 ^ ")"
    | CALL (f, es) =>
        "call " ^ f ^ "(" ^
        String.concat (List.map (fn e => showExp e ^ ",") es) ^ ")"
    | STRING (s) => "\"" ^ s ^ "\""
    | SAMPLE (e1) => "'( " ^ showExp e1 ^ ")"
    | SAMPLES (e1, e2) => "(" ^ showExp e1 ^ " ' " ^ showExp e2 ^ ")"
    | HCONC (e1, e2) => "(" ^ showExp e1 ^ " || " ^ showExp e2 ^ ")"
    | VCONCL (e1, e2) => "(" ^ showExp e1 ^ " |> " ^ showExp e2 ^ ")"
    | VCONCR (e1, e2) => "(" ^ showExp e1 ^ " <| " ^ showExp e2 ^ ")"
    | VCONCC (e1, e2) => "(" ^ showExp e1 ^ " <> " ^ showExp e2 ^ ")"
    | QUESTION (q) => "?" ^ Real.toString q
    | PAIR (e1, e2) => "[" ^ showExp e1 ^ " , " ^ showExp e2 ^ "]"
    | FIRST (e1) => "%1( " ^ showExp e1 ^ ")"
    | SECOND (e1) => "%2( " ^ showExp e1 ^ ")"
    | DEFAULT (x,e1) => "(" ^ x ^" ~ " ^ showExp e1 ^ ")"
end
