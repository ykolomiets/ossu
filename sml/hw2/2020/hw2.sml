(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list


(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare

(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

;

Control.Print.printDepth := 3;
Control.Print.printLength := 3;

use "parsed_small_police.sml";
use "parsed_medium_police.sml";
(*use "parsed_large_police.sml";*)

val large_incident_reports_list =
    case small_incident_reports of
        Array js => js
      | _ => raise (Fail "expected large_incident_reports to be an array")

; Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(**** PUT PROBLEMS 1-5 HERE ****)

(*1*)
fun make_silly_json i =
  let
    fun make_array i =
      if i = 0
      then []
      else Object [("n", Num (int_to_real i)), ("b", True)] :: make_array (i - 1)
  in
    Array (make_array i)
  end

(*2*)
fun concat_with (sep, xs) =
  case xs of
      []        => ""
    | x::[]     => x 
    | x::xs'    => x ^ sep ^ concat_with(sep, xs') 

(*3*)
fun quote_string s = "\"" ^ s ^ "\""

(*4*)
fun real_to_string_for_json r =
  if r < 0.0
  then "-" ^ real_to_string(real_abs r)
  else real_to_string r

(*5*)
fun json_to_string json =
  let
    fun traverse_array xs =
      case xs of
           []           => []
         | x::xs'       => json_to_string x :: traverse_array xs'
    fun traverse_object xs =
      case xs of
           []           => []
         | (n,v)::xs'   => (quote_string(n) ^ " : " ^ json_to_string(v)) :: traverse_object xs'
  in
    case json of
         Num n          => real_to_string_for_json n
       | String s       => quote_string s
       | False          => "false"
       | True           => "true"
       | Null           => "null"
       | Array xs       => "[" ^ concat_with(", ", traverse_array xs) ^ "]"
       | Object xs      => "{" ^ concat_with(", ", traverse_object xs) ^ "}"
  end

(**** PUT PROBLEMS 6-13 HERE ****)

(*6*)
fun assoc (k, xs) =
  case xs of 
       []               => NONE
     |(k1,v1)::xs'      => if k1 = k
                           then SOME v1
                           else assoc (k, xs')

(*7*)
fun dot (j, f) =
  case j of
       Object xs => assoc (f, xs)
     | _ => NONE

(*8*)
fun one_fields json =
  let
    fun get_field_names (xs, acc) =
      case xs of
           []           => acc
         | (n,v)::xs'   => get_field_names (xs', n::acc)
  in
    case json of
         Object xs => get_field_names (xs, [])
       | _  => []
  end

(*9*)
fun no_repeats xs = length xs = length (dedup xs)
  
(*10*)
fun recursive_no_field_repeats j =
  let
    fun traverse_array xs =
      case xs of
           []           => true
         | x::xs'       => recursive_no_field_repeats x andalso traverse_array
         xs'
    fun traverse_object xs=
      case xs of
           []           => true
         | (n, v)::xs'  => recursive_no_field_repeats v andalso traverse_object xs'
  in
    case j of
         Array xs       => traverse_array xs
       | Object xs      => no_repeats(one_fields j) andalso traverse_object xs
       | _ => true
  end


(*11*)
exception SortIsBroken

fun count_occurrences (xs, ex) =
  let
    fun aux (s, count, acc, xs, order) =
      case xs of
           []     => (s, count) :: acc
         | x::xs' => case strcmp(x, s) of
                          EQUAL => aux(s, count + 1, acc, xs', order)
                        | ord => if order = EQUAL orelse ord = order
                           then aux(x, 1, (s, count) :: acc, xs', ord)
                           else raise ex
  in
    case xs of
         []     => []
       | x::xs' => aux (x, 1, [], xs', EQUAL)
  end

(*12*)
fun string_values_for_field (s, xs) =
  case xs of
       []       => []
     | x::xs'   => case dot (x, s) of
                        SOME(String str) => str::string_values_for_field(s, xs')
                      | _ => string_values_for_field(s, xs')

(*13*)
fun filter_field_value (fname, fvalue, jsons) =
  case jsons of
       [] => []
     | x::xs' => case dot (x, fname) of
                      SOME (String str) => if str = fvalue
                                then x::filter_field_value(fname,fvalue,xs')
                                else filter_field_value(fname,fvalue,xs')
                    | _ => filter_field_value(fname, fvalue, xs')
(* histogram_for_field takes a field name f and a list of objects js and
   returns counts for how often a string is the contents of f in js. *)
fun histogram (xs : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

    val sorted_xs = ListMergeSort.sort compare_strings xs
    val counts = count_occurrences (sorted_xs,SortIsBroken)

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2 orelse (n1 = n2 andalso s1 < s2)
  in
    ListMergeSort.sort compare_counts counts
  end

fun histogram_for_field (f,js) =
  histogram (string_values_for_field (f, js))
(**** PUT PROBLEMS 14-18 HERE ****)

(*14*)
fun number_in_histogram (s, xs) =
  case assoc (s, xs) of
       SOME v   => v
     | NONE     => 0
     
(*15*)
val large_event_clearance_description_histogram =
  histogram_for_field("event_clearance_description",
  large_incident_reports_list)

(*16*)
val large_hundred_block_location_histogram =
  histogram_for_field("hundred_block_location", large_incident_reports_list)

(*17*)
val total_shoplifting =
  number_in_histogram("SHOPLIFT",large_event_clearance_description_histogram)

(*18*)
val total_montlake_and_pacific =
  number_in_histogram("MONTLAKE BV NE / NE PACIFIC ST",
  large_hundred_block_location_histogram)


; Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(*19*)
val forty_third_and_the_ave_reports =
  filter_field_value("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list)

(*20*)
val forty_third_and_the_ave_event_clearance_description_histogram =
  histogram_for_field("event_clearance_description",
  forty_third_and_the_ave_reports)

(*21*)
val forty_third_and_the_ave_moving_violations =
  number_in_histogram("TRAFFIC (MOVING) VIOLATION",
  forty_third_and_the_ave_event_clearance_description_histogram)

(*22*)
val noise_disturbance_reports =
  filter_field_value("event_clearance_description", "NOISE DISTURBANCE",
  large_incident_reports_list)

(*23*)
val noise_disturbance_hundred_block_location_histogram =
  histogram_for_field("hundred_block_location",noise_disturbance_reports)

(*24*)
val nineteenth_and_forty_fifth_noise_disturbances = 
  number_in_histogram("45XX BLOCK OF 19TH AVE NE",
  noise_disturbance_hundred_block_location_histogram)

;Control.Print.printDepth := 20;
Control.Print.printLength := 20;

(* For CHALLENGE PROBLEMS, see hw2challenge.sml *)

