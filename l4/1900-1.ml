let rec process (input: string list) (output: string list) (isEven: bool)= 
match (input, isEven) with 
	([], _) -> List.rev output | 
	(h::t, false) -> process t ((h^"0")::(h^"1")::output) true | 
	(h::t, true) -> process t ((h^"1")::(h^"0")::output) false;;

let rec codeIt (list: string list) (bits: int) = 
if bits = 0 then list else codeIt (process list [] true) (bits - 1);;

let code (bits: int) = if bits = 0 then [] else codeIt ["0";"1"] (bits - 1);;

code 1;;
code 2;;
code 3;;
code 4;;
