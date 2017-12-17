type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lTake (index: int) (llist: 'a llist) =
	let rec lTakeIt (i: int) (ll: 'a llist) (nl: 'a list) =
		if i = 0
			then List.rev nl
			else match ll with
				LNil -> List.rev nl |
				LCons(x,xf) -> lTakeIt (i -1) (xf()) (x::nl)
	in lTakeIt index llist [];;
	
let rec toLazy (list: 'a list) =
	match list with
		[] -> LNil |
		h::t -> LCons(h, function () -> toLazy t);;

let rec lFrom (start: int) = LCons(start, function () -> lFrom (start + 1));;
let lTriads =
	let rec lTriadsIt (value: int) = LCons(value, function () -> lTriadsIt ((value+1) mod 3))
	in lTriadsIt 0;;
        
let lSpec = 
	let rec lSpecIt value acc = 
		if acc > 0 
			then LCons (value, function () -> lSpecIt (value) (acc - 1)) 
			else LCons((value + 1), function () -> lSpecIt (value+1) value)
	in lSpecIt 1 1;;

lTake 20 lSpec;;
lTake 20 (lFrom 2);;
lTake 20 lTriads;;

let lSpec2 (values: 'a llist) (counts: int llist) = 
	match (values, counts) with 
		(LNil, _) -> LNil |
		(_, LNil) -> LNil |
		(LCons(fh, ft), LCons(sh, st)) -> let rec lSpecIt (v: 'a llist) (c: int llist) (cv: 'a) (cc: int) =
			if cc > 0
				then LCons(cv, function () -> lSpecIt v c cv (cc - 1))
				else match (v, c) with
					(LNil, _) -> LNil |
					(_, LNil) -> LNil |
					(LCons(fh, ft), LCons(sh, st)) -> lSpecIt (ft()) (st()) fh sh
	in lSpecIt (ft()) (st()) fh sh;;
	
lTake 20 (lSpec2 (lFrom 2) (lFrom 2));;
lTake 20 (lSpec2 (lFrom 2) lTriads);;

let rec lMerge (left: int llist) (right: int llist) =
	match (left, right) with
		(LNil, _) -> right |
		(_, LNil) -> left |
		(LCons(fh, ft), LCons(sh, st)) -> if fh >= sh
			then LCons(sh, function () -> lMerge left (st()))
			else LCons(fh, function () -> lMerge (ft()) right);;
			
toLazy [1;3;5;7;9;11];;
toLazy [2;4;6;8];;
lMerge (toLazy [1;3;5;7;9;11]) (toLazy [2;4;6;8]);;
lTake 20 (lMerge (toLazy [1;3;5;7;9;11]) (toLazy [2;4;6;8]));;

type stefanKopara = Stefan | Kopara;;
type who = STEFANKOPARA of stefanKopara * (unit -> who);;
let rec gimmeStefan = 
	let rec gimmeAnoterOne (stefanKopara: stefanKopara) = 
		match stefanKopara with
			Stefan -> STEFANKOPARA(Stefan, function () -> gimmeAnoterOne Kopara) |
			Kopara -> STEFANKOPARA(Kopara, function () -> gimmeAnoterOne Stefan)
	in gimmeAnoterOne Stefan;;

let takeThem (howMany: int) = 
	let rec takeThemAll (lazyStefans: who) (howManyAgain: int) (allTheStefans: stefanKopara list) =
		if howManyAgain <= 0
			then List.rev allTheStefans
			else match lazyStefans with STEFANKOPARA(stefan, andOthers) -> 
				takeThemAll (andOthers())(howManyAgain - 1) (stefan::allTheStefans)
	in takeThemAll (gimmeStefan) howMany [];;
	
takeThem 100;;

let lFib = 
	let rec lFibIt (first: int) (second: int) = 
		LCons (first + second, function () -> lFibIt second (first + second))
	in LCons(0,function () -> LCons(1, function () -> lFibIt 0 1));;

lTake 20 lFib;;

let lSublist (sublistSize: int) (list: 'a llist) =
	if sublistSize <= 0 then raise(Failure "Illegal argument exception: Sublist size must be greater than 0")
	else let rec sublistIt (l: 'a llist) (c: int) (r: 'a list) =
		if c < sublistSize 
			then match l with
				LNil-> if r = []
					then LNil
					else LCons((List.rev r), function () -> LNil) |
				LCons(x, xf) -> sublistIt (xf()) (c + 1) (x::r)
		else LCons((List.rev r), function () -> sublistIt l 0 [])
	in sublistIt list 0 [];;

lSublist 2 lFib;;
lTake 10 (lSublist 2 lFib);;
lTake 10 (lSublist 3 lFib);;
lTake 10 (lSublist 4 lFib);;

let rec lZip (left: 'a llist) (right: 'a llist) =
	match (left, right) with
		(LCons(fh, ft), LCons(sh, st)) -> LCons((fh, sh), function () -> lZip (ft()) (st())) |
		(LNil, LNil) -> LNil |
		_ -> raise (Failure "Illegal argument exception: Lazy list dimensions does not match");;
		
lTake 20 (lZip lFib (lFrom 0));;

let lUnzip (list: ('a * 'b) llist) =
	let rec unzipFirst (l: ('a * 'b) llist) =
		match l with 
			LNil -> LNil |
			LCons((a,b), xf) -> LCons(a, function () -> unzipFirst (xf()))
	and unzipSecond (l: ('a * 'b) llist) =
		match l with 
			LNil -> LNil |
			LCons((a,b), xf) -> LCons(b, function () -> unzipSecond (xf()))
	in ((unzipFirst list), (unzipSecond list));;
	
let (a,b) = lUnzip (lZip lFib (lFrom 0));;
lTake 20 a;;
lTake 20 b;;

let lDelim (delim: int) (list: 'a llist) =
	let rec lDelimIt (d: int) (l: 'a llist) =
		match l with
			LNil -> LNil |
			LCons(x,xf) -> if d = delim
				then lDelimIt 1 (xf())
				else LCons(x, function () -> lDelimIt (d + 1) (xf()))
	in lDelimIt 1 list;;
	
lTake 20 (lDelim 3 (lFrom 1));;
