(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(*Problem 1*)
(*Part a*)
fun all_except_option(str: string, l: string list) =
  case l of
      [] => NONE
    | x::xs' => if same_string(str, x)
                then SOME xs' (*find same string, skip it and return*)
                else
                  case all_except_option(str, xs') of
                    NONE => NONE
                  | SOME res => SOME ([x] @ res)

(*Part b*)
fun get_substitutions1(ss: string list list, s: string) = 
  case ss of 
      [] => []
    | x::xs' => case all_except_option(s, x) of 
                    NONE => get_substitutions1(xs', s)
                  | SOME res => res @ get_substitutions1(xs', s)

(*Part c*)
fun get_substitutions2(ss: string list list, s: string) =
  let
    fun tail_recur_helper(ss: string list list, s: string, answ: string list) = 
      case ss of 
          [] => answ
        | x::xs' => case all_except_option(s, x) of
                        NONE => tail_recur_helper(xs', s, answ)
                      | SOME res => tail_recur_helper(xs' , s, answ @ res)
  in
    tail_recur_helper(ss, s, [])
  end

(*Part d*)
fun similar_names(ss: string list list, {first=f, middle=m, last=l}) =
  case ss of 
      [] => [{first=f, middle=m, last=l}]
    | x::xs' => let 
                  fun compute_names(ss: string list, m: string, l: string) =
                    case ss of 
                          [] => []
                        | y::ys' => {first = y, middle = m, last = l}::compute_names(ys', m, l)
                in
                (*Once we get the substituation list, compute the result*)
                  {first=f, middle=m, last=l}::compute_names(get_substitutions2(ss, f), m, l)
                end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*Part a*)
fun card_color (Spades, _) = Black
    | card_color (Clubs, _) = Black
    | card_color _ = Red

(*Part b*)
fun card_value (_, Jack) = 11
    | card_value (_, Queen) = 12
    | card_value (_, King) = 13
    | card_value(_, Ace) = 1
    | card_value(_, Num i) = i

(*Part c*)
fun remove_card (cs: card list, c: card, e) = 
    case cs of 
        [] => raise e
      | x::xs' => if x = c 
                  then xs'
                  else x::remove_card(xs', c, e)

(*Part d*)
fun all_same_color(cs: card list) = 
      case cs of
        [] => true
      | _::[] => true
      | x::y::ys' => case (card_color(x), card_color(y)) of 
                            (Black, Black) => all_same_color(y::ys')
                          | (Red, Red) => all_same_color(y::ys')
                          | _ => false

(*Part e*)
fun sum_cards(cs: card list) = 
  let
    fun tail_recur_helper(cs: card list, sum: int) = 
      case cs of
          [] => sum
        | (_, Num i)::xs' => tail_recur_helper(xs', sum + i)
  in
    tail_recur_helper(cs, 0)
  end

(*Part f*)

(* Problem 2 here *)