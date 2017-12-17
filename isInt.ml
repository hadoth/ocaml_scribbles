type iof = Int of int | Float of float;;

let isInt (value: iof) = match value with (Int integer) -> true | (Float floating) -> false;;

isInt(Int 1);;
isInt(Float 1.0);;

let rec differIt (list: iof list) (wasInt: bool) = 
	match list with 
		[] -> true |
		h::t -> if (isInt h != wasInt) then (differIt t (isInt h)) else false;;
		
let rec differ (list: iof list) = match list with
	[] -> true |
	h::t -> differIt t (isInt h);;
	
differ [Int 1; Float 2.0; Int 3; Float 4.0];;
differ [Int 1];;
differ [Float 4.0];;
differ [Int 1; Float 2.0; Int 3; Int 4];;
differ [];;
