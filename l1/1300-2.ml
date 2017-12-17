let rec arithmeticIt (elemToAdd: int) (increment: int) (elements: int) (result: int list) =
	if elements < 0 
		then raise (Failure "Invalid number of elements") 
		else 
			if elements = 0 
			then List.rev result 
			else arithmeticIt (elemToAdd + increment) (increment) (elements - 1) (elemToAdd::result);;
			
let rec arithmetic (startVal: int) (increment: int) (numOfElements) = arithmeticIt startVal increment numOfElements [];;

arithmetic 2 3 4;;
