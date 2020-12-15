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

fun similar_names (string_list_list, {first = first, middle = middle, last = last})=
    let
        val first_name_list = first :: get_substitutions2(string_list_list, first)
        fun helper_func(first_name_list)=
            case first_name_list of
                [] => []
              | xs :: xs' => {first = xs, middle = middle, last = last} :: helper_func(xs')

    in
        helper_func(first_name_list)
    end

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* solutions for problem 2 *)
(* Return the card's color *)
fun card_color card =
    case card of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red
                
(* Return the card's number *)
fun card_value card =
    case card of
        (_, Ace) => 11
      | (_, Num xs) => xs
      | _ => 10
                 
(* Write a functionremove_card, which takes a list of cards cs, a card c, and an exceptione.  It returns a list that has all the elements of cs except c.  If c is in the list more than once, remove only the first one. If c is not in the list, raise the exception e.  You can compare card s with= *)
fun remove_card(card_list, card, ex)=
    let
        fun card_filter card_list=
            case card_list of
                [] => []
              | xs :: xs' => if xs = card then xs' else xs :: card_filter(xs')
        val removed_list = card_filter card_list
    in  
        if removed_list = card_list then
            raise ex
        else
            removed_list
    end

fun all_same_color(card_list)=
    case card_list of
        [] => true
      |  card :: [] => true
      | card_a :: card_b :: card_list' => if card_color card_a = card_color card_b then all_same_color(card_b :: card_list') else false

(* Write a functionsum_cards, which takes a list of cards and returns the sum of their values. Use a locally defined  helper  function  that  is  tail  recursive.  (Take  “calls  use  a  constant  amount  of  stack  space”  as  arequirement for this problem.) *)

fun sum_cards(card_lists)=
    let fun aux(card_list, sum)=
        case card_list of
            [] => sum
          | xs :: xs' => aux(xs', sum + card_value xs )
    in
        aux(card_lists, 0)
    end             
(*Write a function score, which takes a card list(the held-cards) and anint(the goal) and computesthe score as described above. *)

(* The objective is to end the game with a low score (0 is best).  Scoring works as follows:  Let sum be the sum of the values of the held-cards.  If sum is greater than goal, the preliminary score is three times (sum−goal),else the preliminary score is (goal−sum).  The score is the preliminary score unless all the held-cards are the same color, in which case the score is the preliminary score divided by 2 (and rounded down as usual with integer division; use ML’s div operator). *)
fun score(card_lists, goal)=
    let
        val card_sum = sum_cards card_lists
        fun card_cal_helper(sum)=
            if (sum - goal) > 0 then 3 * (sum - goal)
            else
                goal - sum
    in
        if all_same_color card_lists then
            card_cal_helper(card_sum) div 2
        else
            card_cal_helper(card_sum)
    end 

fun officiate(card_list, move_list, goal)=
    let fun produce_help_card (card_list, move_list, held_card, goal)=
            case (move_list, card_list) of
                ([], _) => held_card
              | (Discard x :: ml', _) => produce_help_card(card_list, ml', remove_card(held_card, x, IllegalMove) , goal)
              | (Draw :: ml', []) => held_card
              | (Draw :: ml', card :: card_list') =>
                let val new_held_card = card :: held_card
                in
                    if sum_cards(new_held_card) > goal then
                        new_held_card 
                    else
                        produce_help_card(card_list', ml', new_held_card, goal)
                end
    in
        score(produce_help_card(card_list, move_list, [], goal), goal)
    end
        
