signature Interpreter =
sig

  datatype value = VAL of int list
                 | TEXT of string list
			     | PAIR of value * value

  val seed : int ref
  val seed2 : int ref

  val rollDice : Syntax.Program -> value

  exception RunError of string

  val makeText : value -> value
  val hconc : value*value -> value
  val vconcl : value*value -> value
  val vconcr : value*value -> value
  val vconcc : value*value -> value

end
