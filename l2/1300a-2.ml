let rec addIt (left: 'a list) (right: 'a list) (result: 'a list) (isLeft: bool) = 
match (left, right, isLeft) with 
([], [], _) -> List.rev result |
([], h::t, _) -> addIt left t (h::result) isLeft |
(h::t, [], _) -> addIt t right (h::result) isLeft |
(h::t, _ , true) -> addIt t right (h::result) false |
(_, h::t, false) -> addIt left t (h::result) true;;

let rec add (pair: 'a list * 'a list) = let (left, right) = pair in addIt left right [] true;;

add (['a';'b';'c';'d'],['e';'f';'g';'h';'i';'j']);;
