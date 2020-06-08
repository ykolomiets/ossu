use "hw3.sml";

val test_only_lowercase =
  only_lowercase([]) = [] andalso
  only_lowercase(["abc", "ABC"]) = ["abc"] andalso
  only_lowercase(["ABC"]) = []

val test_longest_string1 = 
  longest_string1([]) = "" andalso
  longest_string1(["abc", "ABCD"]) = "ABCD" andalso
  longest_string1(["abc", "123"]) = "abc"

val test_longest_string2 = 
  longest_string2([]) = "" andalso
  longest_string2(["abc", "ABCD"]) = "ABCD" andalso
  longest_string2(["abc", "123"]) = "123"

val test_longest_string3 = 
  longest_string3([]) = "" andalso
  longest_string3(["abc", "ABCD"]) = "ABCD" andalso
  longest_string3(["abc", "123"]) = "abc"

val test_longest_string4 = 
  longest_string4([]) = "" andalso
  longest_string4(["abc", "ABCD"]) = "ABCD" andalso
  longest_string4(["abc", "123"]) = "123"

val test_longest_lowercase =
  longest_lowercase([]) = "" andalso
  longest_lowercase(["ABC", "BCC"]) = "" andalso
  longest_lowercase(["abc", "ABC", "dce", "DCEF"]) = "abc"

val test_caps_no_X_string =
  caps_no_X_string("AaaCXXXxxDd") = "AAACDD"

val test_first_answer =
  let
    val test = first_answer (fn x => 
                if x mod 2 = 0
                then SOME("even is found : " ^ Int.toString(x))
                else NONE)
  in
    test [1, 2, 3, 4, 5, 6] = "even is found : 2"
  end

val test_all_answers = false

val test_count_wildcards =
  count_wildcards(TupleP [Variable "a", Wildcard, TupleP [Wildcard]]) = 2

val test_count_wild_and_variable_lengths =
  count_wild_and_variable_lengths(TupleP [Variable "a", Wildcard, TupleP [Wildcard]]) = 3

val test_count_a_var =
  count_a_var("123", TupleP [Variable "123", Variable "0"]) = 1

val test_check_pat =
  check_pat (TupleP [Variable "123", Wildcard]) = true andalso
  check_pat (TupleP [Variable "123", Variable "123"]) = false

val test_match =
  match (Const 14, Wildcard) = SOME [] andalso
  match (Const 14, Variable "a") = SOME [("a", Const 14)] andalso
  match (Tuple[Const 14], TupleP [Variable "a"]) = SOME [("a", Const 14)] andalso
  match (Const 14, TupleP []) = NONE andalso
  match (
          Constructor ("ctor", Tuple  [Const 1,  Const 2,      Unit]),
          ConstructorP("ctor", TupleP [Wildcard, Variable "a", UnitP])
        ) = SOME [("a", Const 2)]

val test_first_match = 
  first_match(Const 14, [TupleP [], UnitP, Variable "a", Variable "b"]) = SOME [("a", Const 14)] andalso
  first_match(Const 14, [UnitP]) = NONE 

