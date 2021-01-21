(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* solutions for problem 1 *)

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

fun get_substitutions1 (string_list_list, input_string)=
    case string_list_list of
        [] => []
      | xs :: xs' =>
        let val new_list =  all_except_option(input_string, xs) in
            case new_list of
                NONE => get_substitutions1(xs', input_string)
              | SOME x  => x @ get_substitutions1(xs', input_string)
        end

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
(* We can also use a more compact pattern matching:
fun card_color(c) =
	case c of
		((Clubs | Spades), _) => Black
	   |_ => Red
*)
fun card_color card =
    case card of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | _ => Red
                
fun card_value card =
    case card of
        (_, Ace) => 11
      | (_, Num xs) => xs
      | _ => 10
                 
fun remove_card(card_list, card, ex)=
    (* let *)
    (*     fun card_filter card_list= *)
    (*         case card_list of *)
    (*             [] => [] *)
    (*           | xs :: xs' => if xs = card then xs' else xs :: card_filter(xs') *)
    (*     val removed_list = card_filter card_list *)
    (* in   *)
    (*     if removed_list = card_list then *)
    (*         raise ex *)
    (*     else *)
    (*         removed_list *)
(* end *)
    case card_list of
        [] => raise ex
           | xs :: xs' => if xs = card then xs' else xs :: card_list(xs', card, ex)

fun all_same_color(card_list)=
    case card_list of
        [] => true
      |  card :: [] => true
      | card_a :: card_b :: card_list' => if card_color card_a = card_color card_b then all_same_color(card_b :: card_list') else false


fun sum_cards(card_lists)=
    let fun aux(card_list, sum)=
        case card_list of
            [] => sum
          | xs :: xs' => aux(xs', sum + card_value xs )
    in
        aux(card_lists, 0)
    end             

fun score(card_lists, goal)=
    let
        val card_sum = sum_cards card_lists
        val card_score =  
            if (card_sum - goal) > 0 then 3 * (card_sum - goal)
            else
                goal - card_sum
    in
        if all_same_color card_lists then
            card_score div 2
        else
           card_score 
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
        
