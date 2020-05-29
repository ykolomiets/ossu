(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option1(s, xs) =
  case xs of
       [] => NONE
     | x :: xs' => if same_string(x, s)
                  then SOME(xs')
                  else case all_except_option1(s, xs') of
                            NONE => NONE
                          | SOME ys => SOME(x :: ys)


fun all_except_option2(s, xs) =
  let
    fun internal(xs, ys, found) =
      case xs of
           x :: xs' => if same_string(x, s)
                       then internal(xs', ys, true)
                       else internal(xs', x :: ys, found)
         | _ => if found then SOME(ys) else NONE
  in
    internal(xs, [], false)
  end

fun get_substitutions1(subs, s) =
  case subs of
       xs :: subs' => (case all_except_option1(s, xs) of
                           NONE => get_substitutions1(subs', s)
                         | SOME lst => lst @ get_substitutions1(subs', s))
     | _ => []

fun get_substitutions2(subs, s) =
  let
    fun internal(subs, acc) =
      case subs of
           xs :: subs' => (case all_except_option1(s, xs) of
                           NONE => internal(subs', acc)
                         | SOME lst => internal(subs', acc @ lst))
          | _ => acc
  in
    internal(subs, [])
  end

fun similar_names(subs, full_name) =
  let
    val { first, middle, last } = full_name;
    fun replace_first_name(new_names) =
      case new_names of 
           [] => []
         | new_name :: rest => { first = new_name, middle = middle , last =
         last} :: replace_first_name(rest)
  in
    full_name :: replace_first_name(get_substitutions2(subs, first))
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
