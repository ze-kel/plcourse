(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)


fun all_except_option (s, []) = NONE
  | all_except_option (s, x :: xs) =
    case same_string(s, x) of
	     true => SOME xs
      | false => case all_except_option(s, xs) of
		     NONE => NONE
		   | SOME y => SOME (x :: y)

fun get_substitutions1 ([], s) = []
   | get_substitutions1(cur :: rest, s) =
      case all_except_option(s, cur) of
         NONE => get_substitutions1(rest, s)
         | SOME res => res @ get_substitutions1(rest, s)



fun get_substitutions2([], s) = []
   | get_substitutions2(ol, os) =
   let 
      fun helper(s, [], acc) = acc
         | helper(s, cur :: rest, acc) =
               case all_except_option(s, cur) of
                  NONE => helper(s, rest, acc)
                  | SOME res => helper(s, rest, acc @ res)
   in 
      helper(os, ol, [])
   end



fun similar_names([], bs) = [bs]
   | similar_names(list, {first=f,middle=m,last=l}) =
      let
         val originalName = {first=f, middle=m, last=l}
         val substits = get_substitutions2(list, f)
         fun helper([], acc) = acc
           | helper(cur :: rest, acc) = helper(rest, acc @ [{first=cur, middle=m, last=l}])
      in
         helper(substits, [originalName])
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

fun card_color (card) =
   case card of 
      (Clubs, _) => Black
    | (Spades, _) => Black
    | (_, _) => Red



fun card_value (card ) =
   case card of 
     (_, Ace) => 11
   | (_, Num n) => n  
   | (_, _) => 10

fun remove_card([], card, e) = raise e
   | remove_card(cur :: rest, card, e) =
      case cur = card of
      true => rest
      | false => [cur] @ remove_card(rest, card, e)


fun all_same_color(list) =
   case list of
      [] => true
      | _::[] => true
      | cur::(next::rest) => card_color(cur) = card_color(next) andalso all_same_color(next::rest)

fun sum_cards(list) = 
   let 
      fun helper(acc, []) = acc
         | helper(acc, cur :: rest) = helper(acc + card_value(cur), rest)
   in
   helper(0, list)
   end


fun score(list, goal) = 
   let
      val sumOfList = sum_cards(list)
      val preliminary = case sumOfList > goal of
         true => (sumOfList - goal) * 3
         | false => goal - sumOfList

   in
      case  all_same_color(list) of
         false => preliminary
         | true => preliminary div 2
   end



fun officiate(cardList, moveList, goal) =
   let
      fun nextMove(playerList, deck, []) = score(playerList, goal)
         | nextMove(playerList, deck, curMove :: restMoves) =
             case curMove of
               Discard disCard => nextMove(remove_card(playerList, disCard, IllegalMove), deck, restMoves)
             | Draw => case deck of 
                        [] => score(playerList, goal)
                        | nextCard :: restCards => 
                        let 
                           val newPlayerList = nextCard :: playerList
                           val newSum = sum_cards(newPlayerList)
                        in
                           case (newSum > goal) of
                              true => score(newPlayerList, goal)
                              | false => nextMove(nextCard :: playerList, restCards, restMoves)
                        end

         
   in
      nextMove([], cardList, moveList)
   end

