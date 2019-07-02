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
