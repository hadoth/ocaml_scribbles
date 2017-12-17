let rec removeIt (input: 'a list) (output: 'a list) (counter: int) (counterTemp: int) =
	match (input, counter = counterTemp) with 
		([], _) -> List.rev output |
		(h::t, true) -> removeIt t output counter 1 |
		(h::t, false) -> removeIt t (h::output) counter (counterTemp + 1);;
		
let remove (counter: int) (input: 'a list) = removeIt input [] counter 1;;

remove 3 [1;2;3;4;5;6;7];;
