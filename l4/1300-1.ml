let rec decodeElem (pair: int * 'a) (result: 'a list) = 
	let (i, elem) = pair in if i > 0 then decodeElem ((i - 1), elem) (elem::result) else result;;
	
let rec decodeList (input: (int * 'a) list) (output: 'a list) =
	match input with [] -> (List.rev output) | (h::t) -> (decodeList t (decodeElem h output));;

let decode (list: (int * 'a) list) = decodeList list [];;

decode [(3,'a');(4,'b');(1,'c')];;
