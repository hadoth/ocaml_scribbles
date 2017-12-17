let rec zipIt (left: int list) (right: int list) (result: (int * int) list) = 
	match (left, right) with
		([], []) -> List.rev result |
		(h::t, []) -> zipIt t [] ((h, 0)::result) |
		([], h::t) -> zipIt [] t ((0, h)::result) |
		(lh::lt, rh::rt) -> zipIt lt rt ((lh, rh)::result);;

let zip (left: int list) (right: int list) = zipIt left right [];;

zip [5;4;3;2] [1;2;3;4;5;6];;
