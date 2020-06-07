(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(*1-a*)
fun reverse_list(xs) = 
  let
    fun aux(xs, acc) = 
      case xs of
           [] => acc
         | x :: xs' => aux(xs', x::acc)
  in
    aux(xs, [])
  end

fun all_except_option(s, ss) =
  let
    fun aux(xs, acc, found) =
      case xs of
           x :: xs' => if same_string(x, s)
                       then aux(xs', acc, true)
                       else aux(xs', x :: acc, found)
         | [] => if found then SOME(reverse_list(acc)) else NONE
  in
    aux(ss, [], false)
  end

(*1-b*)
fun get_substitutions1(substitutions, s) =
  case substitutions of
       [] => []
     | ss :: rest => case all_except_option(s, ss) of
                          NONE => get_substitutions1(rest, s)
                        | SOME l => l @ get_substitutions1(rest, s)

(*1-c*)
fun get_substitutions2(substitutions, s) =
  let
    fun aux(substitutions, acc) =
      case substitutions of
           [] => acc
         | ss :: rest => case all_except_option(s, ss) of
                              NONE => aux(rest, acc)
                            | SOME l => aux(rest, acc @ l)
  in
    aux(substitutions, [])
  end

(*1-d*)
fun similar_names(substitutions, full) =
  let
    val {first, middle, last} = full;
    fun map_to_full(similar) =
      case similar of
           [] => []
         | x :: rest => {first=x, middle=middle, last=last} :: map_to_full(rest)
  in
    full :: map_to_full(get_substitutions2(substitutions, first))
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(*2-a*)
fun card_color (s, _) =
  case s of
       Clubs => Black
     | Spades => Black
     | _ => Red

(*2-b*)
fun card_value (_, r) =
  case r of
       Ace => 11
     | Num n => n
     | _ => 10

(*2-c*)
fun remove_card (cs, c, e) =
  case cs of
       [] => raise e
     | x :: xs => if c = x
                 then xs
                 else x :: remove_card(xs, c, e)

(*2-d*)
fun all_same_color cs =
  case cs of
      x::y::rest => card_color(x) = card_color(y) andalso all_same_color(y::rest)
    | _ => true

(*2-e*)
fun sum_cards cs =
  let
    fun internal(cards, sum) =
      case cards of
           [] => sum
         | c::cs => internal(cs, sum + card_value(c))
  in
    internal(cs, 0)
  end

(*2-f*)
fun score (cs, goal) =
  let
    val sum = sum_cards(cs)
    val preliminary_score = if sum > goal then 3 * (sum - goal) else goal - sum
  in
    if all_same_color(cs) 
    then preliminary_score div 2
    else preliminary_score
  end

(*2-g*)
fun officiate(cs, mvs, goal) =
  let
    fun process(cs, held, mvs) = 
      case mvs of
           [] => score(held, goal)
         | Draw::mvs' => (
                      case cs of
                           [] => score(held, goal)
                         | c::cs' =>
                             if sum_cards(c::held) > goal
                             then score(c::held, goal)
                             else process(cs', c::held, mvs')
                   )
         | (Discard c)::mvs' => process(cs, remove_card(held, c, IllegalMove), mvs')
  in
    process(cs, [], mvs)
  end

