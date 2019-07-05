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
	List.foldl(fn (x, y) => if String.size(y) < String.size(x) then x else y) "" l

(*Function 3*)
fun longest_string2(l: string list) = 
	List.foldl(fn (x, y) => if String.size(y) <= String.size(x) then x else y) "" l

(*Functions 4*)
val longest_string_helper = fn x => fn y => fn z => List.foldl(x) y z
fun longest_string3(l) = 
	((longest_string_helper(fn (x,y) => if String.size(y) < String.size(x) then x else y)) "") l
fun longest_string4(l) = 
	((longest_string_helper(fn (x,y) => if String.size(y) <= String.size(x) then x else y)) "") l

(*Functions 5*)
val candidate = Char.isUpper o String.sub
fun longest_capitalized(l: string list) = 
	((longest_string_helper(fn (x, y) => if String.size(y) < String.size(x) andalso candidate(x, 0) then x else y)) "") l

(*Function 6*)
fun rev_string(s: string) = 
	(String.implode o List.rev o String.explode) s

(*Function 7*)
fun first_answer f l = 
	case l of 
			[] => raise NoAnswer
		| x::xs' => case f(x) of 
										SOME v => v
									| NONE => first_answer f xs'

(*Function 8*)
fun all_answers f l =
	let 
		fun tail_recur_helper f l res =
			case l of
					[] => (case res of 
										[] => NONE
									| ans => SOME ans)
				| x::xs' => case f(x) of 
												NONE => NONE
											| SOME v => let
																		val ans = tail_recur_helper f xs' res
																	in
																		case ans of 
																				NONE => NONE
																			| SOME ans => SOME (v @ ans)
																	end
	in
		tail_recur_helper f l []
	end

(*Function 9(a)*)
val count_wildcards = g (fn () => 1) (fn x => 0)

(*Function 9(b)*)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size(x))

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
				| x::xs' => if (List.exists (fn y => if x = y then true else false) xs')
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
		| (UnitP, _) => NONE
		| (ConstP x, Const y) => if x = y then SOME [] else NONE
		| (TupleP l, Tuple v') => if List.length(l) <> List.length(v')
														 then NONE
														 else let val zipl = ListPair.zip(v', l) in (all_answers match) zipl end
		| (ConstructorP(s1,p'), Constructor(s2, v')) => if s1 = s2 
																									 	then match(v', p')
																										else NONE

(*Function 12*)
fun curry f x y = f (x, y)
fun first_match v l = 
	SOME (first_answer (curry match v) l) handle NoAnswer => NONE