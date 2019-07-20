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

(*Function 1*)
fun only_capitals(l: string list) = 
  List.filter(fn x => Char.isUpper(String.sub(x, 0))) l

(*Function 2*)
fun longest_string1(l: string list) =
	List.foldl (fn (x, y) => if String.size(y) < String.size(x) then x else y) "" l

(*Function 3*)
fun longest_string2(l: string list) = 
	List.foldl (fn (x, y) => if String.size(y) <= String.size(x) then x else y) "" l

(*Functions 4*)
fun longest_string_helper f l = 
	List.foldl (fn (x,y) => if f(String.size x, String.size y) then x else y) "" l
val longest_string3 = 
	longest_string_helper (fn (x,y) => x > y)
val longest_string4 = 
	longest_string_helper (fn (x,y) => x >= y)

(*Functions 5*)
fun candidate(l) = List.filter (fn x => Char.isUpper(String.sub(x, 0))) l
val longest_capitalized = longest_string3 o candidate

(*Function 6*)
val rev_string = String.implode o List.rev o String.explode

(*Function 7*)
fun first_answer f l = 
	case l of 
			[] => raise NoAnswer
		| x::xs' => case f(x) of 
										SOME v => v
									| NONE => first_answer f xs'

(*Function 8*)
fun all_answers f l =
	case l of 
			[] => SOME []
		| x::xs' => case f(x) of 
										NONE => NONE
									| SOME v => case (all_answers f xs') of 
																	NONE => NONE 
																| SOME l => SOME (v @ l)

(*Function 9(a)*)
val count_wildcards = g (fn () => 1) (fn x => 0)

(*Function 9(b)*)
val count_wild_and_variable_lengths = g (fn () => 1) String.size

(*Function 9(c)*)
fun count_some_var(s: string, p: pattern) = 
	g (fn () => 0) (fn x => if x = s then 1 else 0) p

(*Function 10*)
fun check_pat p = 
	let
		fun collect_strings p = 
			case p of 
					Variable x => [x]
				| TupleP ps => List.foldl (fn (p, i) => (collect_strings p) @ i) [] ps
				| ConstructorP(_, p) => collect_strings p
				|	_ => []
		fun check_repeats l = 
			case l of 
					[] => true
				| x::xs' => if (List.exists (fn y => x = y) xs')
											then false
											else check_repeats(xs')
	in
		(check_repeats o collect_strings) p
	end

(*Function 11*)
fun match(v: valu, p: pattern) = 
	case (p, v) of 
			(Wildcard, _) => SOME []
		| (Variable x, v') => SOME [(x, v')]
		| (UnitP, Unit) => SOME []
		| (ConstP x, Const y) => if x = y then SOME [] else NONE
		| (TupleP l, Tuple v') => if List.length(l) <> List.length(v')
														 then NONE
														 else all_answers match (ListPair.zip(v', l))
		| (ConstructorP(s1,p'), Constructor(s2, v')) => if s1 = s2 
																									 	then match(v', p')
																										else NONE
		| _ => NONE

(*Function 12*)
fun curry f x y = f (x, y)
fun first_match(v: valu, l: pattern list) = 
	SOME (first_answer (curry match v) l) handle NoAnswer => NONE