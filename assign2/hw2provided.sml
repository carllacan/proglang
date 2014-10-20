(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (str : string, strlst : string list) =
    case strlst of
	[] => NONE
      | h::t => if same_string( h, str) 
		then SOME t 
		else case all_except_option(str, t) of
				   NONE => NONE
				|  SOME a => SOME (h::a)

fun get_substitutions1 (strlstlst : string list list, s : string)=
    let fun is_in_list  (str : string, strlst : string list) =
	    case strlst of
		[] => false
	      | h::t => same_string(str, h) orelse is_in_list(str, t)
    in case strlstlst of
	   [] => []
	 | lh::lt => if is_in_list(s, lh)
		    then valOf (all_except_option(s, lh))@get_substitutions1(lt, s) 
		    else get_substitutions1(lt, s)
    end

fun get_substitutions1 (strstrlst : string list list, s : string)=
    case strstrlst of
	[] => []
      | hd::tl => case all_except_option(s, hd) of
		      NONE => get_substitutions1(tl, s)
		    | SOME l => l@get_substitutions1(tl, s)
						     

fun get_substitutions2 (strstrlst : string list list, s : string)=
    case strstrlst of
	[] => []
      | hd::tl => case all_except_option(s, hd) of
		      NONE => get_substitutions1(tl, s)
		    | SOME l => l@get_substitutions1(tl, s)
						    
fun similar_names (sll : string list list, {first, middle, last})=
    let fun make_names (subs)=
	    case subs of
		[] => []
	      | hd::tl => make_names(tl)@[{first=hd, middle=middle, last=last}]
    in
	case sll of
	    [] => [{first=first, middle=middle, last=last}]
	  | hd::tl => similar_names(tl, {first=first, middle=middle, last=last})@make_names(get_substitutions1([hd], first))
    end
	


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)


fun card_color ((suit, rank))=
    case suit of
	Spades => Black
      | Clubs => Black
      | Diamonds => Red
      | Hearts => Red

fun card_value ((suit, rank))=
    case rank of
	Ace => 11
      | Num x => x
      | _  => 10

fun remove_card (clst : card list, c : card, e)=
    case clst of
	[] => raise e
      | hd::tl => if hd = c then tl else remove_card(tl, c, e)

fun all_same_color (clst : card list) =
    case clst of
	[] => true
      | [card1] => true
      | [card1,card2] => card_color(card1) = card_color(card2)
      | hd::nk::tl => card_color(hd) = card_color(nk) andalso all_same_color(nk::tl)
									     
fun sum_cards (clst : card list)=
    case clst of
	[] => 0
      | hd::tl => card_value(hd)+sum_cards(tl)

fun score (clst : card list, goal : int) = 
    let val preliminary_score =
	    let val sum = sum_cards(clst) 
	    in
		if sum > goal 
		then 3*(sum - goal) 
		else goal - sum
	    end
    in
	if all_same_color(clst) 
	then preliminary_score div 2
	else preliminary_score
    end
	


fun officiate (clst : card list, mlst, goal)=
    let fun round (clst : card list, held, mlst)=
	    case mlst of
		[] => score(held, goal)
	      | (Discard c)::nxt => round(clst, remove_card(held, c, IllegalMove), nxt)
	      | (Draw)::nxt => case clst of
				   [] => score(held,goal)
				 | (top)::rest => if score(top::held, goal) > goal 
						then score(top::held, goal)
						else round(rest, top::held, nxt)


							  
    in
	case (mlst) of
	    ([]) => score([], goal)
	 |  _ => 	round(clst, [], mlst)
    end
