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

val longest_string1 = List.foldl (fn (el, acc) => if String.size(el) > String.size(acc) then el else acc) ""

val longest_string2 = List.foldl (fn (el, acc) => if String.size(el) >= String.size(acc) then el else acc) ""

val longest_string_helper = fn comparer => List.foldl(fn (el, acc) => if comparer(el, acc) then el else acc) ""

val longest_string3 = longest_string_helper(fn (a,b) => String.size(a) > String.size(b))

val longest_string4 = longest_string_helper(fn (a,b) => String.size(a) >= String.size(b))

val longest_capitalized = List.foldl(fn (el, acc) => 
								let 
									val ellen = String.size(el)
									val acclen = String.size(acc)
									val checker = Char.isUpper o String.sub
								in
									if checker(el, 0) andalso ellen > acclen then el else acc
								end
) ""

val rev_string = String.implode o List.rev o String.explode

val first_answer = fn somer => fn list => let
								fun helper(current :: rest) = case somer(current) of
																SOME result => result
															  | NONE => case rest of 
															  			[] => raise NoAnswer
																		| _ => helper(rest)
								in
								case list of 
										[] => raise NoAnswer
										| somelist => helper(somelist)
								end

val all_answers = fn somer => let
									fun folder (el, acc) = case (somer(el), acc) of
															(NONE, SOME[]) => NONE
															| (NONE, _ ) => acc
															| (SOME resp, SOME v) => SOME (v @ resp)
															| (SOME resp, _) => SOME resp
									in
									List.foldl(folder) (SOME [])
									end

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7]
val test9 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7]

val test10 = all_answers (fn x => if x < 4 then SOME [x] else NONE) [2,3,4,5,6,7]

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size(x))

fun count_some_var (str, pat) = g (fn _ => 0) (fn x => if x = str then 1 else 0) pat

fun check_pat (pattern) = let
				fun getVarList (pat) = case pat of 
												  Variable var => [var]
												| TupleP tuple => List.foldl(fn (el, acc) => acc @ getVarList(el) ) [] tuple
												| ConstructorP (name, content) => getVarList(content)
												| _ => []
				fun noReps (list) = case list of 
										 [] => true
										| cur :: rest => if (List.exists(fn el => el = cur) (rest) ) then false else noReps(rest)
						
				val total = noReps o getVarList
				in
					total(pattern)
				end
										 
		

fun match (valu, pattern) = case (valu, pattern) of
						  (_, Wildcard) => SOME []
						| (valu, Variable variab) => SOME [(variab, valu)]
						| (Unit, UnitP) => SOME []
						| (Const a, ConstP b) => if a = b then SOME [] else NONE
						| (Tuple vt, TupleP pt) => if length vt = length pt then all_answers(match) (ListPair.zip(vt, pt)) else NONE
						| (Constructor(name1, content1), ConstructorP(name2, content2)) => if name1 = name2 then match(content1, content2) else NONE
						| _ => NONE

fun first_match value patterns = SOME (first_answer(fn p => match(value, p)) patterns) handle NoAnswer => NONE


val endtest = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard])