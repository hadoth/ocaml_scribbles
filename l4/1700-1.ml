let rec sublistIt (input: 'a list) (output: 'a list) (startR: int) (endR: int) =
	match (input, startR = 0, endR = 0) with 
		(_, true, true) -> List.rev output |
		([], _, _) -> raise (Failure "Index out of bounds") |
		(h::t, false, false) ->  sublistIt t output (startR - 1) (endR - 1) |
		(h::t, true, false) -> sublistIt t (h::output) 0 (endR - 1) |
		(_, false, true) -> raise (Failure "Invalid input; end must be greater or equal to start");;
		
let sublist (input: 'a list) (startR: int) (endR: int) = 
	sublistIt input [] startR endR;;
	
sublist [1;2;3;4;5;6;7;8;9;0] 0 1;;
sublist [1;2;3;4;5;6;7;8;9;0] 0 2;;
sublist [1;2;3;4;5;6;7;8;9;0] 2 4;;
sublist [1;2;3;4;5;6;7;8;9;0] 0 9;;
sublist [1;2;3;4;5;6;7;8;9;0] 10 10;;
	
