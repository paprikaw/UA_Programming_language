(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* solutions for problem 1 *)

(* Write a functionall_except_option, which takes a string and a string list.  Return NONE if the string is not in the list, else return SOME ls twhere ls is identical to the argument list except the stringis not in it.  You may assume the string is in the list at most once.  Usesame_string, provided to you,to compare strings.  Sample solution is around 8 lines. *)

fun all_except_option (input_string, string_list)=
    let
        fun list_without_indentical (input_string, string_list)=
            case string_list of
                [] => [] 
              | str :: string_list' =>
                if same_string (str, input_string) then
                    list_without_indentical (input_string, string_list')
                else str :: list_without_indentical (input_string, string_list')
        val my_list = list_without_indentical(input_string, string_list)
    in
        if my_list = string_list then
            NONE
        else
            SOME my_list
    end

(* Write a function get_substitutions1, which takes astring list list(a list of list of strings, the substitutions) and a strings and returns astring list.  The result has all the strings that are in some list in substitutions that also has, but itself should not be in the result. *)
fun get_substitutions1 (string_list_list, input_string)=
    case string_list_list of
        [] => []
      | xs :: xs' =>
        let val new_list =  all_except_option(input_string, xs) in
            case new_list of
                NONE => get_substitutions1(xs', input_string)
              | SOME x  => x @ get_substitutions1(xs', input_string)
        end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not reall
y come up *)
fun get_substitutions2 (string_list_list, input_string)=
    let fun aux(string_list_list, acc)=
        case string_list_list of
            [] => acc 
        | xs :: xs' =>
            let val new_list =  all_except_option(input_string, xs) in
                case new_list of
                    NONE => aux(xs', acc)
                | SOME x  => aux(xs', acc @ x)
            end
    in
        aux(string_list_list, [])
    end
        
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
