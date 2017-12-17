let rec splitListIt (left: 'a list) (value: 'a) (right: 'a list) (rest: 'a list) (comparator: ('a * 'a) -> bool) = 
	match rest with [] -> (left, value, right) | h::t -> if (comparator (h, value)) then splitListIt left value (h::right) t comparator else splitListIt (h::left) value right t comparator;;
			
let splitList (list: 'a list) (value: 'a) (comparator: ('a * 'a) -> bool) =
	splitListIt [] value [] list comparator;;

let list = [1; 2; 3; 4; 5];;

let compare (pair: int * int) = let (x,y) = pair in x < y;;

splitList list 2 compare;;

let quickSortIt (list: 'a list) (comparator:('a * 'a) -> bool) (result: 'a list)=
	match list with 
		[] -> list |
		h::[] -> list |
		h::t -> let (left, rvalue, right) = splitList t h comparator in
			
