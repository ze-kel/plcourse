(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

val only_capitals = List.filter(fn x => Char.isUpper(String.sub(x, 0)))

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val longest_string1 = List.foldl (fn (el, acc) => if String.size(el) > String.size(acc) then el else acc) ""

val test2 = longest_string1 ["A", "bc", "cd","C"] = "bc"

val longest_string2 = List.foldl (fn (el, acc) => if String.size(el) >= String.size(acc) then el else acc) ""

val test3 = longest_string2 ["A", "bc", "cd","C"] = "cd"

val longest_string_helper = fn comparer => List.foldl(fn (el, acc) => if comparer(el, acc) then el else acc) ""


val longest_string3 = longest_string_helper(fn (a,b) => String.size(a) > String.size(b))

val longest_string4 = longest_string_helper(fn (a,b) => String.size(a) >= String.size(b))

val test4 = longest_string3 ["A", "bc", "cd","C"] = "bc"

val test5 = longest_string4 ["A", "bc", "cd","C"] = "cd"

val longest_capitalized = List.foldl(fn (el, acc) => 
								let 
									val ellen = String.size(el)
									val acclen = String.size(acc)
									val checker = Char.isUpper o String.sub
								in
									if checker(el, 0) andalso ellen > acclen then el else acc
								end
) ""

val test5 = longest_capitalized ["A","bc", "Sex", "Doge", "C"] = "Doge"

