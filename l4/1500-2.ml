let rec rotateIt (input: 'a list) (output: 'a list) (count: int) = 
	if count = 0 
		then output@(List.rev input)
		else match input with [] -> output@(List.rev input) | h::t -> rotateIt t (h::output) (count - 1);;
		
let rotate (count: int) (input: 'a list) = rotateIt (List.rev input) [] (count mod (List.length input));;

rotate 0 [1;2;3;4;5;6;7];;
rotate 1 [1;2;3;4;5;6;7];;
rotate 2 [1;2;3;4;5;6;7];;
rotate 3 [1;2;3;4;5;6;7];;
rotate 4 [1;2;3;4;5;6;7];;
rotate 5 [1;2;3;4;5;6;7];;
rotate 6 [1;2;3;4;5;6;7];;
rotate 7 [1;2;3;4;5;6;7];;
	
