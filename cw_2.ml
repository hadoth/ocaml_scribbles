let comparator a b = a < b;;

let f1 x = x 1 1;;

let f2 x y z = x (y^z);;

let f3 x y z = x y z;;

let f4 x y = function z -> x::y;;

let curr3 f = function (a,b,c) -> f a b c;;

let uncurr3 f a b c = f  (a,b,c);;

let productC (x, y, z) = x * y * z;;

let productU x y z = x * y * z;;

let uncurrProductC = uncurr3 productC;;

let currProductU = curr3 productU;;

productC (1,2,3);;
uncurrProductC 1 2 3;;

productU 4 5 6;;
currProductU (4,5,6);;

let rec fold_left f acc l = match l with [] -> acc | h::t -> fold_left f (f acc h) t;;

let sumProdF (accA, accB) h = (accA + h, accB * h);;

let sumProd list = fold_left sumProdF (0, 1) list;;

sumProd [1;2;3;4;5;6];;

let rec quicksort1 list = 
	match list with 
		[] -> [] |
		[x] -> [x] |
		h::t -> let small = List.filter(fun y -> y < h) t
				and large = List.filter(fun y -> y >= h) t
				in (quicksort1 small)@(h::(quicksort1 large));;
				
quicksort1 [1;1;1;1;];;
quicksort1 [1;2;3;4;];;
quicksort1 [1;4;2;3;];;
quicksort1 [1;4;1;4;];;
quicksort1 [1;4;1;1;];;

let rec insertOne elem list comparator = match list with
	[] -> [elem] |
	h::t -> if comparator elem h
		then elem::(insertSort list comparator)
		else h::(insertSort (elem::t) comparator)
and insertSort list comparator =
	match list with 
		[] -> [] |
		h::t -> insertOne h (insertSort t comparator) comparator;;
		
insertSort [1;2;3] comparator;;
insertSort [1;3;2] comparator;;
insertSort [2;1;3] comparator;;
insertSort [2;3;1] comparator;;
insertSort [3;1;2] comparator;;
insertSort [3;2;3] comparator;;

let rec merge (left: 'a list) (right: 'a list) (comparator: 'a -> 'a -> bool) =
	match (left, right) with
		([], _) -> right |
		(_, []) -> left |
		(lh::lt, rh::rt) -> if comparator lh rh
			then (lh::(merge lt right comparator))
			else (rh::(merge left rt comparator));;
		
let rec halveIt (list: 'a list) (left: 'a list) (right: 'a list) =
	match list with 
		[] -> left, right |
		h::t -> halveIt t (h::right) left;;
		
let halve (list: 'a list) = halveIt list [] [];;

let rec halveIt2 (list: 'a list) (left: 'a list) (counter: int) =
	if counter > 0 
		then match list with h::t -> halveIt2 t (h::left) (counter - 1) | [] -> (list, [])
		else ((List.rev left), list);;
		
let halve2 (list: 'a list) = halveIt2 list [] ((List.length list)/2);;
		
let rec mergeSort (input: 'a list) (comparator: 'a -> 'a -> bool) =
	match input with
		[] -> [] |
		[_] -> input |
		h::t -> 
			let left, right = halve input 
				in merge (mergeSort left comparator) (mergeSort right comparator) comparator;;

let rec mergeSort2 (input: 'a list) (comparator: 'a -> 'a -> bool) =
	match input with
		[] -> [] |
		[_] -> input |
		h::t -> 
			let left, right = halve2 input 
				in merge (mergeSort2 left comparator) (mergeSort2 right comparator) comparator;;

mergeSort [1;2;3] comparator;;
mergeSort [1;3;2] comparator;;
mergeSort [2;1;3] comparator;;
mergeSort [2;3;1] comparator;;
mergeSort [3;1;2] comparator;;
mergeSort [3;2;1] comparator;;

let binComparator (left: int * int) (right: int * int) = let ((a,_),(b,_)) = (left, right) in a<=b;;

let listToSort = [(1,0);(2,1);(3,0);(2,2);(4,0);(2,3);(5,0);(2,4);(6,0);(2,5)];;

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

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

let preorder (tree: 'a bt) =
	let rec preorderIt (t: 'a bt) (r: 'a list) =
		match t with
			Empty -> r |
			Node(x, left, right) -> (x::(preorderIt left (preorderIt right r)))
	in preorderIt tree [];;
	
preorder tt;;

let bfs (tree: 'a bt) =
	let rec bfsTail (treeTail: ('a bt) list) =
		match treeTail with
			[] -> [] |
			Empty::t -> bfsTail t |
			Node(x,lst, rst)::t -> x::(bfsTail (t@lst::rst::[]))
	in bfsTail [tree];;

bfs tt;;
