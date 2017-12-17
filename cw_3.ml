type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lTake (index: int) (list: 'a llist) =
	let rec lTakeIt (i: int) (l: 'a llist) (r: 'a list) =
		if i < index
		then match l with
			LNil -> List.rev r |
			LCons(h, t) -> lTakeIt (i + 1) (t()) (h::r)
		else List.rev r
	in lTakeIt 0 list [];;
	
let rec toLazy (list: 'a list) =
	match list with
		[] -> LNil |
		h::t -> LCons(h, function () -> toLazy t);;
		
let left = toLazy [1;2;3;5;7;9;13;17;18];;
let right = toLazy [2;2;4;6;6;8;10];;

lTake 20 left;;
lTake 20 right;;

let rec lMerge (left: 'a llist) (right: 'a llist) =
	match (left, right) with
		(LNil, LNil) -> LNil |
		(LNil, _) -> right |
		(_, LNil) -> left |
		(LCons(lh,lt), LCons(rh,rt)) ->
			if rh <= lh
				then LCons(rh, function () -> lMerge left (rt()))
				else LCons(lh, function () -> lMerge (lt()) right);;
				
lTake 20 (lMerge right left);;
lTake 20 (lMerge left right);;

let lRepeat (count: int) (list: 'a llist) =
	if count <= 0
		then raise (Failure "Illegal argument exception: element count must be greater than 0")
		else let rec lRepeatIt (c: int) (l: 'a llist) =
			match l with
				LNil -> LNil |
				LCons(h, t) -> if c < count
					then LCons(h, function () -> lRepeatIt (c + 1) l)
					else lRepeatIt 0 (t())
		in lRepeatIt 0 list;;

lTake 40 (lRepeat 3 left);;
lTake 40 (lRepeat 3 right);;


let lFib = 
	let rec lFibIt (first: int) (second: int) =
		LCons(first + second, function () -> lFibIt second (first + second))
	in LCons(0, function () -> LCons(1, function () -> lFibIt 0 1));;
	
lTake 20 lFib;;

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;
type 'a lbt = LEmpty | LNode of 'a * (unit -> 'a lbt) * (unit -> 'a lbt);;


let rec toLazyTree (tree: 'a bt) =
	match tree with
		Empty -> LEmpty |
		Node(e, l, r) -> LNode(e, (function () -> toLazyTree l), (function () -> toLazyTree r));;

let rec lTree (n: int) =
	LNode(n, (function () -> lTree (2*n + 1)), (function () -> lTree (2*n + 2)));;
	
let rec lConcat (left: 'a llist) (right: 'a llist) =
	match left with
		LNil -> right |
		LCons(h, t) -> LCons(h, function () -> (lConcat (t()) right));;
		
lTake 20 (lConcat left right);;

let lBfs (tree: 'a lbt) =
	let rec lBfsTail (tail: ('a lbt) llist) =
		match tail with
			LNil -> LNil |
			LCons(LEmpty, next) -> lBfsTail (next()) |
			LCons(LNode(x, left, right), next) -> 
				let lListOfTwo (l: 'a lbt) (r: 'a lbt) =
					LCons(l, (function () -> LCons(r, (function () -> LNil))))
				in LCons(x, (function () -> lBfsTail (lConcat (next()) (lListOfTwo (left()) (right())))))
	in lBfsTail (LCons(tree, (function () -> LNil)));;

let tt = Node(1,
	Node(2,
		Node(4,
			Empty,
			Empty
		),
		Empty
	),
	Node(3,
		Node(5,
			Empty,
			Node(6,
				Empty,
				Empty
			)
		),
		Empty
	)
);;

toLazyTree tt;;
lBfs (toLazyTree tt);;
lTake 20 (lBfs (toLazyTree tt));;
lTake 20 (lBfs (lTree 2));;
lTake 200 (lBfs (lTree 2));;
