(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

infix |>
fun x |> f = f x

infix $
fun f $ g = fn x => g(f x)

(*1*)
val only_lowercase = List.filter (fn x => String.sub(x, 0) |> Char.isLower)

(*2*)
val longest_string1 = foldl (fn (s, longest) => 
                                if String.size s > String.size longest
                                then s
                                else longest) "" 

(*3*)
val longest_string2 = foldl (fn (s, longest) => 
                                if String.size s >= String.size longest
                                then s
                                else longest) "" 

(*4*)
fun longest_string_helper f xs = foldl (fn (s1, s2) => 
                                        if f (s1, s2)
                                        then s1
                                        else s2) "" xs
(*5*)
val longest_string3 = longest_string_helper (fn (s1, s2) => String.size s1 > String.size s2)
val longest_string4 = longest_string_helper (fn (s1, s2) => String.size s1 >= String.size s2)

(*6*)
val longest_lowercase = longest_string1 o only_lowercase

(*7*)
fun caps_no_X_string s =
  s
  |> String.explode
  |> List.map Char.toUpper
  |> List.filter (fn c => c <> #"X")
  |> String.implode

fun caps_no_X_string s = 
  s
  |> String.explode
  |> List.foldl (fn (c, acc) =>
      if c = #"X" orelse c = #"x"
      then acc
      else Char.toUpper c :: acc) []
  |> List.rev
  |> String.implode

(*8*)
fun first_answer f xs =
  case xs of
       [] => raise NoAnswer
     | x::xs' => case f x of
                      SOME a => a
                    | NONE => first_answer f xs'

(*9*)
fun all_answers f xs =
  let
    fun aux (xs, acc) =
      case xs of
           [] => SOME(acc)
         | x::xs' => case f x of
                          SOME a => aux(xs', acc @ a)
                        | NONE => NONE
  in
    aux(xs, [])
  end

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

(*10*)
(*
 *
 *
 *
 *)

(*11*)
val count_wildcards = g (fn () => 1) (fn x => 0)

(*12*)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

(*13*)
fun count_a_var (s, p) = 
  g (fn () => 0) (fn x => if x = s then 1 else 0) p

(*14*)
fun check_pat p =
  let
    fun get_all_strings p =
      case p of
           Variable x => [x]
         | TupleP ps => List.foldl (fn (p, acc) => acc @ get_all_strings p) [] ps
         | ConstructorP (_, p) => get_all_strings p
         | _ => []
    fun all_unique xs =
      case xs of 
           [] => true
         | x::xs' => not (List.exists(fn y => y=x) xs') andalso all_unique xs'
  in
    p |> get_all_strings |> all_unique
  end

(*15*)
fun match (v, p) =
  case (v, p) of
       (Const x, ConstP y)      => if x = y then SOME [] else NONE
     | (x, Variable s)          => SOME [(s, v)]
     | (Unit, UnitP)            => SOME []
     | (Constructor (x_name, x_value), ConstructorP (y_name, y_pattern))
          => if x_name = y_name then match(x_value, y_pattern) else NONE
     | (Tuple vs, TupleP ps)    => 
         all_answers match (ListPair.zip(vs, ps))
     | (v, Wildcard)            => SOME []
     | _                        => NONE

(*16*)
fun first_match (v, ps) =
  SOME (first_answer (fn p => match(v, p)) ps)
  handle NoAnswer => NONE

datatype typ = Anything
       | UnitT
       | IntT
       | TupleT of typ list
       | Datatype of string

(*17*)

