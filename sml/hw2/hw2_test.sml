use "hw2.sml";

val test_all_except_option = 
  all_except_option("a", []) = NONE andalso
  all_except_option("a", ["a"]) = SOME([]) andalso
  all_except_option("a", ["1", "2", "a", "3"]) = SOME(["1", "2", "3"])

val test_get_substitutions1 =
  get_substitutions1([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie",
  "Fred", "F"]], "Fred") = ["Fredrick", "Freddie", "F"]
  
val test_get_substitutions2 =
  get_substitutions2([["Fred", "Fredrick"], ["Elizabeth", "Betty"], ["Freddie", "Fred", "F"]], "Fred") = ["Fredrick", "Freddie", "F"]

val test_card_color =
  card_color(Clubs, Jack) = Black andalso
  card_color(Diamonds, Queen) = Red andalso
  card_color(Spades, King) = Black andalso
  card_color(Hearts, Ace) = Red 

val test_card_value =
  card_value(Clubs, Jack) = 10 andalso
  card_value(Diamonds, Queen) = 10 andalso
  card_value(Spades, King) = 10 andalso
  card_value(Hearts, Ace) = 11 andalso
  card_value(Hearts, Num 5) = 5

val test_remove_card =
  remove_card([(Clubs, Jack)], (Clubs, Jack), IllegalMove) = [] andalso
  remove_card([(Clubs, Jack), (Clubs, Jack)], (Clubs, Jack), IllegalMove) = [(Clubs, Jack)] andalso
  remove_card([(Clubs, Queen), (Clubs, Jack)], (Clubs, Jack), IllegalMove) = [(Clubs, Queen)]

val test_all_same_color =
  all_same_color([]) andalso
  all_same_color([(Spades, Jack)]) andalso
  all_same_color([(Clubs, Jack), (Spades, Jack)]) andalso
  all_same_color([(Clubs, Jack), (Spades, Jack), (Spades, Queen)]) andalso
  all_same_color([(Diamonds, Jack), (Spades, Jack)]) = false

val test_sum_cards =
  sum_cards([]) = 0 andalso
  sum_cards([(Spades, Jack)]) = 10 andalso
  sum_cards([(Clubs, Jack), (Spades, Jack)]) = 20 andalso
  sum_cards([(Clubs, Jack), (Spades, Jack), (Spades, Ace)]) = 31 andalso
  sum_cards([(Diamonds, Num 5), (Spades, Jack)]) = 15

val test_officiate =
  officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6 andalso
  officiate ([(Clubs,Ace), (Spades, Ace), (Clubs, Ace), (Spades,
 Ace)], [Draw,Draw,Draw,Draw,Draw], 42) = 3 andalso
  officiate([(Clubs,Jack),(Spades,Num(8))], [Draw,Discard(Clubs,Jack)], 42) = 21
