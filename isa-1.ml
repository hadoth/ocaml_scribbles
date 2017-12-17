type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec ltake llist = match llist with 
	(0, _) -> [] |
	(_, LNil) -> [] |
	(n, LCons(x,xf)) -> x::ltake(n-1, xf());;

let rec lSpecIt value acc = 
	if acc > 0 
		then LCons (value, function ()-> lSpecIt (value) (acc - 1)) 
		else LCons((value + 1), function () -> lSpecIt (value+1) value);;
		
let lSpec = lSpecIt 1 1;;
		
ltake (20, lSpec);;
