let rec insertRest (tail: 'a list) (revHead: 'a list) = 
	match tail with [] -> List.rev revHead | h::t -> insertRest t (h::revHead);;

insertRest [3;4;5] [2;1];;
insertRest [] [5;4;3;2;1];;
insertRest [1;2;3;4;5] [];;
	
let rec insertOne (tail: 'a list) (revHead: 'a list) (comparator: ('a * 'a) -> bool) (elem: 'a) = 
	match tail with 
		[] -> List.rev (elem::revHead) | 
		h::t -> if (comparator (h, elem)) 
			then insertOne t (h::revHead) comparator elem
			else insertRest tail (elem::revHead);;

let comparator (pair: int * int) = let (left, right) = pair in left < right;;

insertOne [1;2;4;5;6] [] comparator 3;;

let rec insertSortIt (input: 'a list) (output: 'a list) (comparator: ('a * 'a) -> bool) = 
	match input with [] -> output | h::t -> insertSortIt t (insertOne output [] comparator h) comparator;;
	
let insertSort (input: 'a list) (comparator: ('a * 'a) -> bool) = insertSortIt input [] comparator;;

insertSort [1;4;2;3;7;4;123;1;34;23;54;23;67;9787;34;23;45;562;23;13;1234;34] comparator;;
