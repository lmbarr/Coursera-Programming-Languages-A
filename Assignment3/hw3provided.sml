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

(*1*)
fun only_capitals string_list =
	List.filter (fn s => Char.isUpper(String.sub(s, 0))) string_list

(*2*)
fun longest_string1 string_list =
	foldl (fn (s1, s2) => if String.size s1 > String.size s2
						  then s1
						  else s2) "" string_list

(*3*)
fun longest_string2 string_list =
foldl (fn (s1, s2) => if String.size s1 >= String.size s2
					  then s1
					  else s2) "" string_list

(*4*)
fun longest_string_helper f string_list =
	foldl (fn (s1, s2) => if f(String.size s1, String.size s2) then s1 else s2) "" string_list
val longest_string3 = longest_string_helper (fn (x, y) => x > y)
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(*5*)
val longest_capitalized = fn string_list => (longest_string3 o only_capitals) string_list

(*6*)
fun rev_string s = (String.implode o List.rev o String.explode) s

(*7*)
fun first_answer f lista =
	case lista of
	  [] => raise NoAnswer
	| head::tail => case f head of
					  NONE => first_answer f tail
					| SOME v => v

(*8*)
fun all_answers f lista =
	let fun helper(acc, lista) =
			case lista of
			 [] => SOME acc
		    | head::tail => case f head of
					          NONE => NONE
							| SOME v => helper(v @ acc, tail)
	in
	helper([], lista)
	end

(*9a*)
fun count_wildcards p = g (fn () => 1) (fn x => 0) p

(*9b*)
fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size x) p

(*9c*)
fun count_some_var(s, p) = g (fn () => 0) (fn x => if x = s then 1 else 0) p

(*10*)
fun check_pat p =
	let fun get_string_list p =
			case p of
			  Variable s => [s]
			| TupleP pattern_list => List.foldl (fn (p,i) => (get_string_list p) @ i) [] pattern_list
			| ConstructorP(s, pt) => get_string_list pt
			| _ => []

		fun exist_repetitions list_string =
			case list_string of
			  [] => false
			| head::tail => List.exists (fn s => s = head) tail orelse (exist_repetitions tail)
	in
		not((exist_repetitions o get_string_list) p)
	end

(*11*)
fun match(v, p) =
	case (v, p) of
	  (_, Wildcard) => SOME []
	| (v, Variable s) => SOME [(s, v)]
	| (Unit, UnitP) => SOME []
	| (Const x, ConstP y) => if x = y then SOME [] else NONE
	| (Tuple valu_list, TupleP pattern_list) => if length valu_list = length pattern_list
		then all_answers match (ListPair.zip(valu_list, pattern_list)) else NONE
	| (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v,p) else NONE
	| (_, _) => NONE

(*12*)
fun first_match v pattern_list =
	SOME(first_answer(fn p => match(v, p)) pattern_list) handle NoAnswer => NONE
