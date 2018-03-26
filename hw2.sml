(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* a *)
fun all_except_option (mystring, sl) =
    let
	fun breakpoint (currentlist,pastlist) =
	    case currentlist of
		[] => NONE
	     |  s :: sl' => if same_string(mystring,s) then
			       SOME (pastlist @ sl')
			   else
			       breakpoint (sl',s :: pastlist)
    in
	breakpoint (sl,[])
    end

(* b *)
fun get_substitutions1 (substitutions,s) =
    case substitutions of
	[] => []
     |  sub::sub' => case all_except_option(s,sub) of
			    NONE => get_substitutions1(sub',s)
			  | SOME i => i @ get_substitutions1(sub',s)
							    
(* c *)
fun get_substitutions2 (substitutions,s) =
    let fun in_subs (subsubs,lst) =
	    case subsubs of
		[] => lst
	     |  sub::sub' => case all_except_option(s,sub) of
				 NONE => in_subs (sub',lst)
			       | SOME i => in_subs (sub',i @ lst)
    in
	in_subs (substitutions,[])
    end
	
(* d *)
fun similar_names (substitutions,{first=x,middle=y,last=z}) =
    let
	val lst_of_subs = get_substitutions2(substitutions,x)
    in
	let fun repeat (currentstr, n_acc) =
		case currentstr of
		    [] => n_acc
		  | c::c' => repeat(c',{first=c,last=z,middle=y} :: n_acc)
	in
	    repeat(lst_of_subs,[{first=x,last=z,middle=y}])
	end
    end

	
(* 2.a *)
fun card_color c =
    case c of
	((Clubs | Spades),_) => Black
      | (_,_) => Red

(* b *)
fun card_value c =
    case c of
	(_,Num i) => i
      | (_,(Jack | Queen | King)) => 10
      | (_,Ace) => 11

(* c *)
fun remove_card (cs,c,e)  =
    let
	fun run_remove (current_hand,ending_hand) =
	    case current_hand of
		[] => raise e
	      | x::x' => if x = c
			 then ending_hand @ x'
			 else run_remove (x',x :: ending_hand)
    in
	run_remove (cs,[])
    end
	
(* d *)
fun all_same_color cs =
    case cs of
	[] => true
      | c::[] => true
      | c::(c'::c'') => (card_color(c) = card_color(c')) andalso
			all_same_color(c'::c'')

(* e *)
fun sum_cards cs =
    let
	fun running_total(card_list,tot) =
	    case card_list of
		[] => tot
	      | c::c' => running_total(c',tot + card_value(c))
    in
	running_total(cs,0)
    end
	
(* f *)
fun score (cs,goal) =
    let val sum = sum_cards(cs)
	val same = all_same_color(cs)
    in
	case (sum>goal,same) of
	    (true,false) => 3 * (sum - goal)
	  | (false,false) => goal - sum
	  | (true,_) => (3 * (sum - goal)) div 2
	  | (false,_) => (goal - sum) div 2
    end
	

(* g *)
fun officiate (cs,ms,goal) =
  let
      fun gaming (held_cards,moves,card_list) =
	if sum_cards held_cards > goal then
	    score(held_cards,goal)
	else
	    case moves of
		[] => score(held_cards,goal)
	     |  mv::mv' => case mv of
			       Discard i => gaming (remove_card(held_cards,i,IllegalMove),mv',card_list)
			     | Draw => case card_list of
					   [] => score(held_cards,goal)
					 | cl::cl' => gaming (cl :: held_cards,mv',cl')
  in
      gaming([],ms,cs)
  end
      
