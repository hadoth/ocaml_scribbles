let rec forYieldIt (input: 'a list) (output: 'a list) (predicate: 'a -> bool) (transform: 'a -> 'b) =
match input with 
	[] -> List.rev output | 
	h::t -> if (predicate h) 
		then forYieldIt t ((transform h)::output) predicate transform
		else forYieldIt t output predicate transform;;

let forYield (list: 'a list) (predicate: 'a -> bool) (transform: 'a -> 'b) = 
	forYieldIt list [] predicate transform;;
	
let isEven (value: int) = (value mod 2) = 0;;

let double (value: int) = value * 2;;

forYield [1;2;3;4] isEven double;;
