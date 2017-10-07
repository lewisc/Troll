(* rescopes let-expressions to speed up probability calculations *)

(* count the number of occurrences of a variable x in an expression *)
fun occurrences x exp =
    case exp of
      Syntax.NUM _ => 0
    | Syntax.ID (y,p) => if x=y then 1 else 0
    | Syntax.EMPTY => 0
    | Syntax.CONC (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.FROMTO (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.CHOOSE (e1,p) => occurrences x e1
    | Syntax.DIFFERENT (e1,p) => occurrences x e1
    | Syntax.PLUS (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.MINUS (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.TIMES (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.DIVIDE (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.MOD (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.UMINUS (e1,p) => occurrences x e1
    | Syntax.D (e1,p) => occurrences x e1
    | Syntax.Z (e1,p) => occurrences x e1
    | Syntax.SIGN (e1,p) => occurrences x e1
    | Syntax.SUM (e1,p) => occurrences x e1
    | Syntax.COUNT (e1,p) => occurrences x e1
    | Syntax.LEAST (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.LARGEST (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.MINIMAL (e1,p) => occurrences x e1
    | Syntax.MAXIMAL (e1,p) => occurrences x e1
    | Syntax.HASH (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.AND (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.EQ (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.NEQ (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.LT (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.GT (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.LE (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.GE (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.DROP (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.KEEP (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.PICK (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.SETMINUS (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.MEDIAN (e1,p) => occurrences x e1
    | Syntax.LET (y,e1,e2,p) =>
        if y=x then occurrences x e1
        else occurrences x e1 + occurrences x e2
    | Syntax.REPEAT (y,e1,e2,continue,p) =>
        if y=x then 2*occurrences x e1
        else 2*(occurrences x e1 + occurrences x e2)
    | Syntax.ACCUM (y,e1,e2,continue,p) =>
        if y=x then 2*occurrences x e1
        else 2*(occurrences x e1 + occurrences x e2)
    | Syntax.FOREACH (y,e1,e2,p) =>
        if y=x then occurrences x e1
        else occurrences x e1 + 2*occurrences x e2
    | Syntax.IF (e1,e2,e3,p) =>
        occurrences x e1 + Int.max (occurrences x e2, occurrences x e3)
    | Syntax.CALL (f,args,p) =>
        List.foldr (op +) 0 (List.map (occurrences x) args)
    | Syntax.STRING _ => 0
    | Syntax.SAMPLE (e1,p) => occurrences x e1
    | Syntax.SAMPLES (e1,e2,p) => occurrences x e1 + 2*(occurrences x e2)
    | Syntax.HCONC (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.VCONCL (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.VCONCR (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.VCONCC (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.QUESTION _ => 0
    | Syntax.PAIR (e1,e2,p) => occurrences x e1 + occurrences x e2
    | Syntax.FIRST (e1,p) => occurrences x e1
    | Syntax.SECOND (e1,p) => occurrences x e1
    | Syntax.DEFAULT (y,e1,p) =>
         if x=y then 2 + occurrences x e1 else occurrences x e1
         (* since y can not be substituted, we count it as occuring twice *)

infixr 6 ++
infixr 5 --

fun member y [] = false
  | member y (x :: xs) = x=y orelse member y xs

fun [] ++ ys = ys
  | (x :: xs) ++ ys = if member x ys then xs ++ ys else x :: (xs ++ ys)

fun [] -- y = []
  | (x :: xs) -- y = if y=x then xs -- y else x :: (xs -- y)

fun freeVars exp =
    case exp of
      Syntax.NUM _ => []
    | Syntax.ID (y,p) => [y]
    | Syntax.EMPTY => []
    | Syntax.CONC (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.FROMTO (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.CHOOSE (e1,p) => freeVars e1
    | Syntax.DIFFERENT (e1,p) => freeVars e1
    | Syntax.PLUS (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.MINUS (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.TIMES (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.DIVIDE (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.MOD (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.UMINUS (e1,p) => freeVars e1
    | Syntax.D (e1,p) => freeVars e1
    | Syntax.Z (e1,p) => freeVars e1
    | Syntax.SIGN (e1,p) => freeVars e1
    | Syntax.SUM (e1,p) => freeVars e1
    | Syntax.COUNT (e1,p) => freeVars e1
    | Syntax.LEAST (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.LARGEST (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.MINIMAL (e1,p) => freeVars e1
    | Syntax.MAXIMAL (e1,p) => freeVars e1
    | Syntax.HASH (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.AND (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.EQ (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.NEQ (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.LT (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.GT (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.LE (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.GE (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.DROP (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.KEEP (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.PICK (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.SETMINUS (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.MEDIAN (e1,p) => freeVars e1
    | Syntax.LET (y,e1,e2,p) =>
        freeVars e1 ++ (freeVars e2 -- y)
    | Syntax.REPEAT (y,e1,e2,continue,p) =>
        freeVars e1 ++ (freeVars e2 -- y)
    | Syntax.ACCUM (y,e1,e2,continue,p) =>
        freeVars e1 ++ (freeVars e2 -- y)
    | Syntax.FOREACH (y,e1,e2,p) =>
        freeVars e1 ++ (freeVars e2 -- y)
    | Syntax.IF (e1,e2,e3,p) =>
        freeVars e1 ++ freeVars e2 ++ freeVars e3
    | Syntax.CALL (f,args,p) =>
        List.foldr (op ++) [] (List.map freeVars args)
    | Syntax.STRING _ => []
    | Syntax.SAMPLE (e1,p) => freeVars e1
    | Syntax.SAMPLES (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.HCONC (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.VCONCL (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.VCONCR (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.VCONCC (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.QUESTION _ => []
    | Syntax.PAIR (e1,e2,p) => freeVars e1 ++ freeVars e2
    | Syntax.FIRST (e1,p) => freeVars e1
    | Syntax.SECOND (e1,p) => freeVars e1
    | Syntax.DEFAULT (y, e1,p) => [y] ++ freeVars e1

(* tests if an expression introduces randomness *)
fun isRandom exp =
    case exp of
      Syntax.NUM _ => false
    | Syntax.ID (y,p) => false
    | Syntax.EMPTY => false
    | Syntax.CONC (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.FROMTO (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.CHOOSE (e1,p) => true
    | Syntax.DIFFERENT (e1,p) => isRandom e1
    | Syntax.PLUS (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.MINUS (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.TIMES (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.DIVIDE (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.MOD (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.UMINUS (e1,p) => isRandom e1
    | Syntax.D (e1,p) => true
    | Syntax.Z (e1,p) => true
    | Syntax.SIGN (e1,p) => isRandom e1
    | Syntax.SUM (e1,p) => isRandom e1
    | Syntax.COUNT (e1,p) => isRandom e1
    | Syntax.LEAST (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.LARGEST (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.MINIMAL (e1,p) => isRandom e1
    | Syntax.MAXIMAL (e1,p) => isRandom e1
    | Syntax.HASH (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.AND (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.EQ (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.NEQ (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.LT (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.GT (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.LE (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.GE (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.DROP (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.KEEP (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.PICK (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.SETMINUS (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.MEDIAN (e1,p) => isRandom e1
    | Syntax.LET (y,e1,e2,p) =>
        isRandom e1 orelse isRandom e2
    | Syntax.REPEAT (y,e1,e2,continue,p) =>
        isRandom e1 orelse isRandom e2
    | Syntax.ACCUM (y,e1,e2,continue,p) =>
        isRandom e1 orelse isRandom e2
    | Syntax.FOREACH (y,e1,e2,p) =>
        isRandom e1 orelse isRandom e2
    | Syntax.IF (e1,e2,e3,p) =>
        isRandom e1 orelse isRandom e2 orelse isRandom e3
    | Syntax.CALL (f,args,p) => true (* can be random *)
    | Syntax.STRING _ => false
    | Syntax.SAMPLE (e1,p) => isRandom e1
    | Syntax.SAMPLES (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.HCONC (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.VCONCL (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.VCONCR (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.VCONCC (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.QUESTION _ => true
    | Syntax.PAIR (e1,e2,p) => isRandom e1 orelse isRandom e2
    | Syntax.FIRST (e1,p) => isRandom e1
    | Syntax.SECOND (e1,p) => isRandom e1
    | Syntax.DEFAULT (y, e1,p) => isRandom e1

(* substitute x by exp1 in exp2 *)
fun substitute x exp1 exp2 =
    case exp2 of
      Syntax.NUM _ => exp2
    | Syntax.ID (y,p) => if x=y then exp1 else exp2
    | Syntax.EMPTY => exp2
    | Syntax.CONC (e1,e2,p) =>
        Syntax.CONC (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.FROMTO (e1,e2,p) =>
        Syntax.FROMTO (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.CHOOSE (e1,p) => Syntax.CHOOSE (substitute x exp1 e1,p)
    | Syntax.DIFFERENT (e1,p) => Syntax.DIFFERENT (substitute x exp1 e1,p)
    | Syntax.PLUS (e1,e2,p) =>
        Syntax.PLUS (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.MINUS (e1,e2,p) =>
        Syntax.MINUS (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.TIMES (e1,e2,p) =>
        Syntax.TIMES (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.DIVIDE (e1,e2,p) =>
        Syntax.DIVIDE (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.MOD (e1,e2,p) =>
        Syntax.MOD (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.UMINUS (e1,p) => Syntax.UMINUS (substitute x exp1 e1,p)
    | Syntax.D (e1,p) => Syntax.D (substitute x exp1 e1,p)
    | Syntax.Z (e1,p) => Syntax.Z (substitute x exp1 e1,p)
    | Syntax.SIGN (e1,p) => Syntax.SIGN (substitute x exp1 e1,p)
    | Syntax.SUM (e1,p) => Syntax.SUM (substitute x exp1 e1,p)
    | Syntax.COUNT (e1,p) => Syntax.COUNT (substitute x exp1 e1,p)
    | Syntax.LEAST (e1,e2,p) =>
        Syntax.LEAST (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.LARGEST (e1,e2,p) =>
        Syntax.LARGEST (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.MINIMAL (e1,p) => Syntax.MINIMAL (substitute x exp1 e1,p)
    | Syntax.MAXIMAL (e1,p) => Syntax.MAXIMAL (substitute x exp1 e1,p)
    | Syntax.HASH (e1,e2,p) =>
        Syntax.HASH (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.AND (e1,e2,p) =>
        Syntax.AND (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.EQ (e1,e2,p) =>
        Syntax.EQ (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.NEQ (e1,e2,p) =>
        Syntax.NEQ (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.LT (e1,e2,p) =>
        Syntax.LT (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.GT (e1,e2,p) =>
        Syntax.GT (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.LE (e1,e2,p) =>
        Syntax.LE (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.GE (e1,e2,p) =>
        Syntax.GE (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.DROP (e1,e2,p) =>
        Syntax.DROP (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.KEEP (e1,e2,p) =>
        Syntax.KEEP (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.PICK (e1,e2,p) =>
        Syntax.PICK (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.SETMINUS (e1,e2,p) =>
        Syntax.SETMINUS (substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.MEDIAN (e1,p) => Syntax.MEDIAN (substitute x exp1 e1,p)
    | Syntax.LET (y,e1,e2,p) =>
        if y=x then
          Syntax.LET (y, substitute x exp1 e1, e2, p)
        else
          Syntax.LET (y, substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.REPEAT (y,e1,e2,continue,p) =>
        if y=x then
          Syntax.REPEAT (y, substitute x exp1 e1, e2,continue, p)
        else
          Syntax.REPEAT (y, substitute x exp1 e1, substitute x exp1 e2,
			 continue, p)
    | Syntax.ACCUM (y,e1,e2,continue,p) =>
        if y=x then
          Syntax.ACCUM (y, substitute x exp1 e1, e2,
			continue, p)
        else
          Syntax.ACCUM (y, substitute x exp1 e1, substitute x exp1 e2,
			continue, p)
    | Syntax.FOREACH (y,e1,e2,p) =>
        if y=x then
          Syntax.FOREACH (y, substitute x exp1 e1, e2, p)
        else
          Syntax.FOREACH (y, substitute x exp1 e1, substitute x exp1 e2, p)
    | Syntax.IF (e1,e2,e3,p) =>
        Syntax.IF (substitute x exp1 e1,
		   substitute x exp1 e2,
		   substitute x exp1 e3, p)
    | Syntax.CALL (f,args,p) =>
        Syntax.CALL (f, List.map (substitute x exp1) args, p)
    | Syntax.STRING _ => exp2
    | Syntax.SAMPLE (e1,p) => Syntax.SAMPLE (substitute x exp1 e1,p)
    | Syntax.SAMPLES (e1,e2,p) =>
        Syntax.SAMPLES (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.HCONC (e1,e2,p) =>
        Syntax.HCONC (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.VCONCL (e1,e2,p) =>
        Syntax.VCONCL (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.VCONCR (e1,e2,p) =>
        Syntax.VCONCR (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.VCONCC (e1,e2,p) =>
        Syntax.VCONCC (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.QUESTION _ => exp2
    | Syntax.PAIR (e1,e2,p) =>
        Syntax.PAIR (substitute x exp1 e1,substitute x exp1 e2,p)
    | Syntax.FIRST (e1,p) => Syntax.FIRST (substitute x exp1 e1,p)
    | Syntax.SECOND (e1,p) => Syntax.SECOND (substitute x exp1 e1,p)
    | Syntax.DEFAULT (y,e1,p) => Syntax.DEFAULT (y,substitute x exp1 e1,p)
        (* y can in theory not be substituted *)

(* splits expression e containing x into maximal context c[] not containing x
   and expression e' that does, such that e = c[e'].
   Returns SOME (c,e') if e contains x, NONE otherwise *)
fun context x fv e =
  case e of
    Syntax.NUM _ => NONE
  | Syntax.ID (y,p) => if x=y then SOME (fn a => a, e) else NONE
  | Syntax.EMPTY => NONE
  | Syntax.CONC (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.CONC (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.CONC (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.FROMTO (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.FROMTO (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.FROMTO (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.CHOOSE (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.CHOOSE (c a,p), e'))
  | Syntax.DIFFERENT (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.DIFFERENT (c a,p), e'))
  | Syntax.PLUS (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.PLUS (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.PLUS (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.MINUS (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.MINUS (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.MINUS (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.TIMES (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.TIMES (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.TIMES (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.DIVIDE (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.DIVIDE (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.DIVIDE (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.MOD (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.MOD (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.MOD (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.UMINUS (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.UMINUS (c a,p), e'))
  | Syntax.D (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.D (c a,p), e'))
  | Syntax.Z (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.Z (c a,p), e'))
  | Syntax.SIGN (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.SIGN (c a,p), e'))
  | Syntax.SUM (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.SUM (c a,p), e'))
  | Syntax.COUNT (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.COUNT (c a,p), e'))
  | Syntax.LEAST (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.LEAST (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.LEAST (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.LARGEST (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.LARGEST (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.LARGEST (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.MAXIMAL (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.MAXIMAL (c a,p), e'))
  | Syntax.MINIMAL (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.MINIMAL (c a,p), e'))
  | Syntax.HASH (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.HASH (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.HASH (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.AND (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.AND (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.AND (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.EQ (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.EQ (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.EQ (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.NEQ (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.NEQ (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.NEQ (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.LT (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.LT (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.LT (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.GT (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.GT (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.GT (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.LE (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.LE (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.LE (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.GE (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.GE (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.GE (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.DROP (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.DROP (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.DROP (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.KEEP (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.KEEP (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.KEEP (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.PICK (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.PICK (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.PICK (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.SETMINUS (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.SETMINUS (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.SETMINUS (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.MEDIAN (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.MEDIAN (c a,p), e'))
  | Syntax.LET (y,e1,e2,p) =>
      if x=y orelse member y fv then
        case context x fv e1 of
          NONE => NONE
        | SOME (c, e') => SOME (fn a => Syntax.LET (y, c a, e2, p), e')
      else
        (case (context x fv e1, context x fv e2) of
           (NONE, NONE) => NONE
         | (NONE, SOME (c, e')) =>
             SOME (fn a => Syntax.LET (y, e1, c a, p), e')
         | (SOME (c, e'), NONE) =>
             SOME (fn a => Syntax.LET (y, c a, e2, p), e')
         | _ => SOME (fn a => a, e))
  | Syntax.REPEAT (y,e1,e2,b,p) =>
      if x=y orelse member y fv then
        case context x fv e1 of
          NONE => NONE
        | SOME (c, e') => SOME (fn a => Syntax.REPEAT (y, c a, e2, b, p), e')
      else
        (case (context x fv e1, context x fv e2) of
           (NONE, NONE) => NONE
         | (SOME (c, e'), NONE) =>
             SOME (fn a => Syntax.REPEAT (y, c a, e2, b, p), e')
         | _ => SOME (fn a => a, e))
  | Syntax.ACCUM (y,e1,e2,b,p) =>
      if x=y orelse member y fv then
        case context x fv e1 of
          NONE => NONE
        | SOME (c, e') => SOME (fn a => Syntax.ACCUM (y, c a, e2, b, p), e')
      else
        (case (context x fv e1, context x fv e2) of
           (NONE, NONE) => NONE
         | (SOME (c, e'), NONE) =>
             SOME (fn a => Syntax.ACCUM (y, c a, e2, b, p), e')
         | _ => SOME (fn a => a, e))
  | Syntax.FOREACH (y,e1,e2,p) =>
      if x=y orelse member y fv then
        case context x fv e1 of
          NONE => NONE
        | SOME (c, e') => SOME (fn a => Syntax.FOREACH (y, c a, e2, p), e')
      else
        (case (context x fv e1, context x fv e2) of
           (NONE, NONE) => NONE
         | (SOME (c, e'), NONE) =>
             SOME (fn a => Syntax.FOREACH (y, c a, e2, p), e')
         | _ => SOME (fn a => a, e))
  | Syntax.IF (e1,e2,e3,p) =>
      (case (context x fv e1, context x fv e2, context x fv e3) of
         (NONE, NONE, NONE) => NONE
       | (NONE, NONE, SOME (c, e')) =>
           SOME (fn a => Syntax.IF (e1, e2, c a, p), e')
       | (NONE, SOME (c, e'), NONE) =>
           SOME (fn a => Syntax.IF (e1, c a, e3, p), e')
       | (SOME (c, e'), NONE, NONE) =>
           SOME (fn a => Syntax.IF (c a, e2, e3, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.CALL (f,es,p) =>
      let
        val cs = List.map (context x fv) es
      in
        if List.all (fn NONE => true | _ => false) cs then NONE
        else SOME (fn a => a, e)
      end
  | Syntax.STRING _ => NONE
  | Syntax.SAMPLE (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.SAMPLE (c a,p), e'))
  | Syntax.SAMPLES (e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.SAMPLES (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.SAMPLES (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.HCONC(e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.HCONC (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.HCONC (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.VCONCL(e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.VCONCL (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.VCONCL (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.VCONCR(e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.VCONCR (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.VCONCR (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.VCONCC(e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.VCONCC (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.VCONCC (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.QUESTION _ => NONE
  | Syntax.PAIR(e1,e2,p) =>
      (case (context x fv e1, context x fv e2) of
         (NONE, NONE) => NONE
       | (NONE, SOME (c, e')) => SOME (fn a => Syntax.PAIR (e1, c a, p), e')
       | (SOME (c, e'), NONE) => SOME (fn a => Syntax.PAIR (c a, e2, p), e')
       | _ => SOME (fn a => a, e))
  | Syntax.FIRST (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.FIRST (c a,p), e'))
  | Syntax.SECOND (e1,p) =>
      (case context x fv e1 of
         NONE => NONE
       | SOME (c, e') => SOME (fn a => Syntax.SECOND (c a,p), e'))
  | Syntax.DEFAULT (y,e1,p) =>
      if x=y then SOME (fn a => a, e)
      else
        (case context x fv e1 of
           NONE => NONE
         | SOME (c, e') => SOME (fn a => Syntax.DEFAULT (y,c a,p), e'))




(* rescope let-expressions to speed up probability calculations *)
fun rescope e =
  case e of
    (Syntax.LET (x,e1,e2,p)) =>
       let
         val e1' = rescope e1
         val e2' = rescope e2
         val fv = freeVars e1'
       in
         case occurrences x e2' of
           0 => e2'  (* no occurrences: eliminate *)
         | 1 => (* one occurrence: substitute if random *)
            if isRandom e1'
            then substitute x e1' e2'
            else Syntax.LET (x,e1',e2',p) (* no gain by substituting *)
                                          (* enable combining lets (see below) *)
         | _ => (* multiple occurrences: move binding maximally into e2' *)
            case context x fv e2' of
              NONE => e2' (* no occurrences, redundant *)
            | SOME (c, e') => (* move inside context *)
                case e' of
                  Syntax.LET (y, e3, e4, q) => (* combine lets *)
                    c (Syntax.LET
                         (x ^"+"^ y,
                          Syntax.LET
                            (x, e1', Syntax.PAIR (Syntax.ID (x, p), e3, q), p),
                          Syntax.LET
                            (x,
                             Syntax.FIRST (Syntax.ID (x ^"+"^ y, p), p),
                             Syntax.LET
                               (y,
                                Syntax.SECOND (Syntax.ID (x ^"+"^ y, q), q),
                                e4,
                                q),
                             p),
                          p))
                | _ => c (Syntax.LET (x,e1',e',p))
       end
  | Syntax.NUM _ => e
  | Syntax.ID _ => e
  | Syntax.EMPTY => e
  | Syntax.CONC (e1, e2, p) => Syntax.CONC (rescope e1, rescope e2, p)
  | Syntax.FROMTO (e1, e2, p) => Syntax.FROMTO (rescope e1, rescope e2, p)
  | Syntax.CHOOSE (e1,p) => Syntax.CHOOSE (rescope e1, p)
  | Syntax.DIFFERENT (e1,p) => Syntax.DIFFERENT (rescope e1, p)
  | Syntax.PLUS (e1, e2, p) => Syntax.PLUS (rescope e1, rescope e2, p)
  | Syntax.MINUS (e1, e2, p) => Syntax.MINUS (rescope e1, rescope e2, p)
  | Syntax.TIMES (e1, e2, p) => Syntax.TIMES (rescope e1, rescope e2, p)
  | Syntax.DIVIDE (e1, e2, p) => Syntax.DIVIDE (rescope e1, rescope e2, p)
  | Syntax.MOD (e1, e2, p) => Syntax.MOD (rescope e1, rescope e2, p)
  | Syntax.UMINUS (e1,p) => Syntax.UMINUS (rescope e1, p)
  | Syntax.D (e1,p) => Syntax.D (rescope e1, p)
  | Syntax.Z (e1,p) => Syntax.Z (rescope e1, p)
  | Syntax.SIGN (e1,p) => Syntax.SIGN (rescope e1, p)
  | Syntax.SUM (e1,p) => Syntax.SUM (rescope e1, p)
  | Syntax.COUNT (e1,p) => Syntax.COUNT (rescope e1, p)
  | Syntax.LEAST (e1, e2, p) => Syntax.LEAST (rescope e1, rescope e2, p)
  | Syntax.LARGEST (e1, e2, p) => Syntax.LARGEST (rescope e1, rescope e2, p)
  | Syntax.MINIMAL (e1,p) => Syntax.MINIMAL (rescope e1, p)
  | Syntax.MAXIMAL (e1,p) => Syntax.MAXIMAL (rescope e1, p)
  | Syntax.HASH (e1, e2, p) => Syntax.HASH (rescope e1, rescope e2, p)
  | Syntax.AND (e1, e2, p) => Syntax.AND (rescope e1, rescope e2, p)
  | Syntax.EQ (e1, e2, p) => Syntax.EQ (rescope e1, rescope e2, p)
  | Syntax.NEQ (e1, e2, p) => Syntax.NEQ (rescope e1, rescope e2, p)
  | Syntax.LT (e1, e2, p) => Syntax.LT (rescope e1, rescope e2, p)
  | Syntax.GT (e1, e2, p) => Syntax.GT (rescope e1, rescope e2, p)
  | Syntax.LE (e1, e2, p) => Syntax.LE (rescope e1, rescope e2, p)
  | Syntax.GE (e1, e2, p) => Syntax.GE (rescope e1, rescope e2, p)
  | Syntax.DROP (e1, e2, p) => Syntax.DROP (rescope e1, rescope e2, p)
  | Syntax.KEEP (e1, e2, p) => Syntax.KEEP (rescope e1, rescope e2, p)
  | Syntax.PICK (e1, e2, p) => Syntax.PICK (rescope e1, rescope e2, p)
  | Syntax.SETMINUS (e1, e2, p) => Syntax.SETMINUS (rescope e1, rescope e2, p)
  | Syntax.MEDIAN (e1,p) => Syntax.MEDIAN (rescope e1, p)
  | Syntax.REPEAT (x, e1, e2, b, p) =>
	  Syntax.REPEAT (x, rescope e1, rescope e2, b, p)
  | Syntax.ACCUM (x, e1, e2, b, p) =>
	  Syntax.ACCUM (x, rescope e1, rescope e2, b, p)
  | Syntax.FOREACH (x, e1, e2, p) =>
	  Syntax.FOREACH (x, rescope e1, rescope e2, p)
  | Syntax.IF (e1, e2, e3, p) =>
      Syntax.IF (rescope e1, rescope e2, rescope e3, p)
  | Syntax.CALL (f, es, p) => Syntax.CALL (f, List.map rescope es, p)
  | Syntax.STRING _ => e
  | Syntax.SAMPLE (e1,p) => Syntax.SAMPLE (rescope e1, p)
  | Syntax.SAMPLES (e1, e2, p) => Syntax.SAMPLES (rescope e1, rescope e2, p)
  | Syntax.HCONC (e1, e2, p) => Syntax.HCONC (rescope e1, rescope e2, p)
  | Syntax.VCONCL (e1, e2, p) => Syntax.VCONCL (rescope e1, rescope e2, p)
  | Syntax.VCONCR (e1, e2, p) => Syntax.VCONCR (rescope e1, rescope e2, p)
  | Syntax.VCONCC (e1, e2, p) => Syntax.VCONCC (rescope e1, rescope e2, p)
  | Syntax.QUESTION _ => e
  | Syntax.PAIR (e1, e2, p) => Syntax.PAIR (rescope e1, rescope e2, p)
  | Syntax.FIRST (e1,p) => Syntax.FIRST (rescope e1, p)
  | Syntax.SECOND (e1,p) => Syntax.SECOND (rescope e1, p)
  | Syntax.DEFAULT (y,e1,p) => Syntax.DEFAULT (y,rescope e1, p)
