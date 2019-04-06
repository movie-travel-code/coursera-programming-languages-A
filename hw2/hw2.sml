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

(*Problem 2*)
(*Part a*)
fun card_color (Spades, _) = Black
    | card_color (Clubs, _) = Black
    | card_color _ = Red

(*Part b*)
fun card_value (_, Jack) = 10
    | card_value (_, Queen) = 10
    | card_value (_, King) = 10
    | card_value(_, Ace) = 11
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
        | x::xs' => tail_recur_helper(xs', sum + card_value(x))
  in
    tail_recur_helper(cs, 0)
  end

(*Part f*)
fun score(cs: card list, goal: int) = 
  let 
    val sum = sum_cards(cs)
    val is_same_color = all_same_color(cs)
  in
    if sum > goal 
    then if is_same_color
          then (sum - goal) * 3 div 2
          else (sum - goal) * 3
    else if is_same_color
          then (goal - sum) div 2
          else goal - sum
  end

fun officiate(cs: card list, ml: move list, goal: int) = 
  let
    fun tail_recur_helper(cs: card list, ml: move list, goal: int, hc: card list) = 
      case (cs, ml) of 
        ([], _) => score(hc, goal)
        | (_, []) => score(hc, goal)
        | (h::cs', m::ml') => case m of
                            Discard c => tail_recur_helper(cs, ml', goal, remove_card(hc, c, IllegalMove))
                          | Draw => if sum_cards(h::hc) > goal 
                                    then score(h::hc, goal)
                                    else tail_recur_helper(cs', ml', goal, h::hc)

  in
    tail_recur_helper(cs, ml, goal, [])
  end

(*Challenge problem 3*)
(*part a*)
(*TODO we can make it a tail recur version*)
fun count_ace(cs: card list) = 
  case cs of 
      [] => 0
    | x::xs' => case x of 
                  (_, Ace) => count_ace(xs') + 1
                | _ => count_ace(xs')

fun score_challenge(cs: card list, goal: int) = 
  let
    val max_sum = sum_cards(cs)
    val min_sum = sum_cards(cs) - count_ace(cs) * 10
    val d_value = goal mod 10 - max_sum mod 10

    val is_same_color = all_same_color(cs)
  in
    (*We should compute that how much Aces choose value 1 can be closest to the goal*)
    if max_sum <= goal
    then score(cs, goal) (*All aces use value 11*)
    else if min_sum >= goal 
          then if is_same_color (*All aces use value 11*)
                then (min_sum - goal) * 3 div 2
                else (min_sum - goal) * 3
          else (*Some use value 11, some uses value 1*)
            let
              val lower = if d_value <= 0
                          then d_value + 10
                          else d_value
              val greater = if d_value <= 0
                            then 0 - d_value
                            else 10 - d_value
            in
              if is_same_color
              then Int.min(greater * 3, lower) div 2
              else Int.min(greater * 3, lower)
            end
  end

fun officiate_challenge(cs: card list, ml: move list, goal: int) = 
  let
    fun tail_recur_helper(cs: card list, ml: move list, goal: int, hc: card list) = 
      case (cs, ml) of 
        ([], _) => score_challenge(hc, goal)
        | (_, []) => score_challenge(hc, goal)
        | (h::cs', m::ml') => case m of
                            Discard c => tail_recur_helper(cs, ml', goal, remove_card(cs, c, IllegalMove))
                          | Draw => if sum_cards(h::hc) - count_ace(h::hc) * 10 > goal 
                                    then score_challenge(h::hc, goal)
                                    else tail_recur_helper(cs', ml', goal, h::hc)
  in
    tail_recur_helper(cs, ml, goal, [])
  end

(*part b*)
(*Select every card from the held cards to discard, can we get zero score from discarding it and draw a new one*)
fun discard_and_draw(cs: card list, goal: int, c: card, ml: move list) =
  let
    fun helper(cs_help: card list, cs: card list, c: card, ml: move list) = 
      case cs_help of 
          [] => NONE
        | x::xs' => if officiate(cs@[c], ml@[Discard(x)]@[Draw], goal) = 0
                    then SOME x
                    else helper(xs', cs, c, ml)
  in
    helper(cs, cs, c, ml)
  end

(*We must draw the first card from the card list, but we can discard any card from the held list.*)
fun careful_player(cs: card list, goal: int) =
  let
    fun helper(cs: card list, goal: int, ml: move list, hc: card list, score: int) = 
      case cs of 
          [] => ml (*There is no more card to draw, return*)
        | c::cs' => if score = 0
                    then ml (*If the score is 0, return*)
                    else if score < 0
                          then
                              case discard_and_draw(hc, goal, c, ml) of 
                                  NONE => helper(cs', goal, ml@[Draw], hc@[c], officiate(hc@[c], ml@[Draw], goal))
                                | SOME x => ml@[Discard x]@[Draw]
                          else
                                if goal - sum_cards(hc) > 10
                                then helper(cs', goal, ml@[Draw], hc@[c], officiate(hc@[c], ml@[Draw], goal))
                                else 
                                  case discard_and_draw(hc, goal, c, ml) of 
                                      NONE => helper(cs', goal, ml@[Draw], hc@[c], officiate(hc@[c], ml@[Draw], goal))
                                    | SOME x => ml@[Discard x]@[Draw]
  in
    helper(cs, goal, [], [], officiate([], [], goal))
  end
