use "HW03.sml";
(* Homework3 Simple Test*)

(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_verbose = only_capitals ["Abc", "bbb", "C"] = ["Abc", "C"]
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_empty = longest_string1 [] = ""
val test2_tie = longest_string1 ["A", "B", "C"] = "A"

val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_tie = longest_string2 ["C", "B", "A"] = "A"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1
val test9a_constructor = count_wildcards (ConstructorP ("Value", Wildcard) )= 1
val test9a_Tuple = count_wildcards (TupleP [ConstP 1, ConstP 2, Wildcard, TupleP [ConstP 1, ConstP 2, Wildcard]]) = 2

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_verbose = count_wild_and_variable_lengths (TupleP [Variable("ab"), Variable("ab")]) = 4
val test9b_verbose2 = count_wild_and_variable_lengths (TupleP[Wildcard,ConstP 17,Variable "x",UnitP,TupleP[UnitP,UnitP,UnitP],ConstructorP ("",UnitP),TupleP[],ConstructorP ("wild",Wildcard)]) = 3

val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_verbose = count_some_var ("x", TupleP [Variable("x"), Variable("s"), Variable("x")]) = 2

val test10 = check_pat (Variable("x")) = true
val test10_verbose = check_pat (ConstructorP ("hi", TupleP[Variable "x",ConstructorP ("yo",TupleP[Variable "x",UnitP])])) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_verbose = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE
val test12 = first_match Unit [UnitP] = SOME []
(*

*)
