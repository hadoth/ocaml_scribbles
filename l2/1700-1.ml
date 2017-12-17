let rec separation (list: int list) (mod1: int list) (mod2: int list) (rest: int list) =
match list with 
	[] -> ((List.rev mod1), (List.rev mod2), (List.rev rest)) |
	h::t -> match (h mod 10) with 
		1 -> separation t (h::mod1) mod2 rest |
		2 -> separation t mod1 (h::mod2) rest |
		_ -> separation t mod1 mod2 ((5 + h)::rest);;
		
let separate (list: int list) = separation list [] [] [];;

separate [0;11;22;33];;
