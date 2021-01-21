(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

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

fun only_capitals string_list=
    List.filter (fn str => Char.isUpper(String.sub (str, 0))) string_list

fun longest_string1 string_list=
    List.foldl (fn (x, acc) => if String.size x > String.size acc then x else acc) "" string_list

fun longest_string2 string_list=
    List.foldl (fn (x, acc) => if String.size x >= String.size acc then x else acc) "" string_list

fun longest_string_helper f string_list=
        List.foldl (fn (x, acc) => if f(String.size x, String.size acc) then x else acc) "" string_list 

val longest_string3 = longest_string_helper (fn (x, y) =>  x > y)
val longest_string4 = longest_string_helper (fn (x, y) =>  x >= y)

fun longest_capitalized string_list=
    (longest_string3 o only_capitals) string_list
    
fun rev_string string=
    (String.implode o List.rev o String.explode) string

fun first_answer f list=
    case list of
        [] => raise NoAnswer
      | xs :: xs'=> case f xs of
                        NONE => first_answer f xs'
                      | SOME v => v 

fun all_answers f list=
    let fun helper acc list=
            case list of
                [] => SOME acc 
              | xs :: xs'=> case f xs of
                                NONE => NONE 
                              | SOME v => helper (acc @ v) xs'
    in
        helper [] list
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

fun count_wildcards p=
    case p of
        (Wildcard | ConstructorP(_, Wildcard)) => 1
      | TupleP (xs :: xs') => count_wildcards(xs) + count_wildcards(TupleP xs')
      | _ => 0

fun count_wild_and_variable_lengths p=
    case p of
        (Wildcard | ConstructorP(_, Wildcard)) => 1
      | TupleP (xs :: xs') => count_wild_and_variable_lengths(xs) + count_wild_and_variable_lengths(TupleP xs')
      | Variable str => String.size str
      | _ => 0

fun count_some_var (str, p)=
    case p of
        (TupleP (xs :: xs') | ConstructorP (_, TupleP (xs :: xs'))) => count_some_var(str, xs) + count_some_var(str, TupleP xs')
         | (Variable string | ConstructorP (_, Variable string)) => if str = string then 1 else 0  
         | _ => 0
    
fun check_pat p=
    let
        fun Var_list p=
            case p of
                (Variable str | ConstructorP (_, Variable str)) => [str]
              | (TupleP list | ConstructorP (_, TupleP list)) => List.foldl (fn (x, acc) => acc @ Var_list(x)) [] list
              | _ => []

        fun if_repeats string_list=
            case string_list of
                [] => true
              | xs :: xs' => if List.exists (fn y => y = xs) xs' then false else if_repeats xs'
    in
        (if_repeats o Var_list) p
    end

fun match var_and_p =
    case var_and_p of
        ((_, Wildcard) | (Unit, UnitP)) => SOME []
      | (Const i1, ConstP i2) => if i1 = i2 then SOME [] else NONE
      | (var, Variable s) => SOME [(s, var)]
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps then all_answers (fn (x, y) => match (x, y)) (ListPair.zip (vs, ps)) else NONE
      | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match (v, p) else NONE
      | _ =>  NONE

fun first_match var pattern_list=
    (* case pattern_list of *)
    (*     [] => NONE *)
    (*   | xs :: xs' => case match(var, xs) of *)
    (*                         SOME x => SOME x *)
    (*                      | NONE => first_match var xs' *)
    SOME (first_answer (fn x => match(var, x)) pattern_list)
    handle NoAnswer => NONE
