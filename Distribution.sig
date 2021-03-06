signature Distribution =
sig

  type value = Interpreter.value (* was int list *)
 
  type pValue = value * real

  val distribDice : Syntax.Program -> pValue list

  exception DistribError of string

  val maxiterations : int ref

end
