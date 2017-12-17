let rec divideIt (source: int list) (mod0: int list) (mod1: int list) (mod2: int list) (state: int) = match (source, state) with
	([], _) -> (List.rev mod1, List.rev mod2, List.rev mod0) |
	(h::t, 0) -> divideIt t ((h + 5)::mod0) mod1 mod2 1 |
	(h::t, 1) -> divideIt t mod0 (h::mod1) mod2 2 |
	(h::t, 2) -> divideIt t mod0 mod1 (h::mod2) 0 |
	(_, _) -> raise (Failure "Modulo 3 must be of value 0, 1 or 2!");;
	
let divide (source: int list) = divideIt source [] [] [] 0;;

divide [0;11;22;33];;
