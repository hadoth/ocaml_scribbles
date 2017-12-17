let ends (list: 'a list) = match list with 
	[] -> raise (Failure "Cannot return ends of empty list") |
	[elem] -> (elem, elem) |
	h::t -> (h, (List.hd (List.rev t)));;

ends [];;
ends ['a'];;
ends [5;4;3;2;1];;

let rec drop (list: 'a list) (count: int) =
	if count < 1 
		then list
		else match list with
			[] -> raise (Failure "Illegal argument exception: Number of elements to remove exceeds the size of list") |
			h::t -> drop t (count - 1);;
			
drop [] 0;;
drop [] 1;;
drop ['a';'b';'c'] 0;;
drop ['a';'b';'c'] 1;;
drop ['a';'b';'c'] 2;;
drop ['a';'b';'c'] 3;;
drop ['a';'b';'c'] 4;;

let rec isSorted (list: 'a list) = 
	let rec isSortedIt (first: 'a) (anotherList: 'a list) (isAsc: bool) = 
		match anotherList with
			[] -> true |
			h::t -> if first = h 
				then isSortedIt h t isAsc
				else if isAsc = (h > first)
					then isSortedIt h t isAsc
					else false
	in match list with
		[] -> true |
		[_] -> true |
		f::s::[] -> true |
		f::s::t -> if f = s
			then isSorted (s::t)
			else isSortedIt s t (s > f);;
			
isSorted [];;
isSorted [1];;
isSorted [1;1;1;1;1;1;1;1;1];;
isSorted [1;1;1;1;1;1;1;1;2];;
isSorted [2;1;1;1;1;1;1;1;1];;
isSorted [5;4;3;2;1];;
isSorted [1;2;3;4;5];;
isSorted [1;1;2;2;3;3;4;4;5;5];;
isSorted [5;5;4;4;3;3;2;2;1;1];;

isSorted [2;1;1;1;1;1;1;1;2];;
isSorted [5;4;3;4;2;1];;
isSorted [1;2;3;2;4;5];;
isSorted [1;2;3;4;5;4];;
isSorted [5;4;3;2;1;2];;
	
let pwr (base: int) (count: int) =
	let rec pwrOne (c: int) (r: int) =
		if c > 0 then pwrOne (c - 1) (r * base) else r
	in let rec pwrIt (c: int) (r: int list) =
		if c > 0 then pwrIt (c - 1) ((pwrOne (c-1) 1)::r) else r
	in if count < 0
		then raise (Failure "Illegal argument exception: Number of list elements cannot be negative")
		else pwrIt count [];;
		
pwr 1 0;;
pwr 1 1;;
pwr 1 5;;
pwr 2 0;;
pwr 2 1;;
pwr 2 5;;
pwr 2 (-1);;

let split (list: 'a list) (separator: 'a) =
	let rec splitIt (l: 'a list) (lower: 'a list) (higher: 'a list) =
		match l with 
			[] -> (List.rev lower, List.rev higher) |
			h::t -> if h <= separator
				then splitIt t (h::lower) higher
				else splitIt t lower (h::higher)
	in splitIt list [] [];;

split [1;8;3;2;5] 0;;
split [1;8;3;2;5] 1;;
split [1;8;3;2;5] 2;;
split [1;8;3;2;5] 3;;
split [1;8;3;2;5] 4;;
split [1;8;3;2;5] 5;;
split [1;8;3;2;5] 6;;
split [1;8;3;2;5] 7;;
split [1;8;3;2;5] 8;;
split [1;8;3;2;5] 9;;
split [] 0;;

let segments (list: 'a list) (length: int) =
	if length <= 0 then raise (Failure "Illegal argument exception: sublist length must be greater than 0") else
	let rec segmentIt (l: 'a list) (small: 'a list) (big: ('a list) list) (count: int) =
		match l with 
			[] -> List.rev ((List.rev small)::big) |
			h::t -> if count = length
				then segmentIt t [h] ((List.rev small)::big) 1
				else segmentIt t (h::small) big (count + 1)
	in segmentIt list [] [] 0;;
	
segments [1;2;9;4;5;6;7;8;3] 2;;
segments [1;2;9;4;5;6;7;8] 2;;
segments [3] 2;;
segments [] 2;;
segments [] 0;;

let flatten (list: ('a list) list) =
	let rec flattenIt (lol: ('a list) list) (l: 'a list) (r: 'a list) =
		match (lol, l) with
			([],[]) -> List.rev r |
			(h::t, []) -> flattenIt t h r |
			(_, h::t) -> flattenIt lol t (h::r)
	in flattenIt list [] [];;

flatten [[5;6]; [1;2;3]];;

let count (list: 'a list) (elem: 'a) = 
	let rec countIt (l: 'a list) (r: int) =
		match l with
			[] -> r |
			h::t -> if h = elem
				then countIt t (r + 1)
				else countIt t r
	in countIt list 0;;
	
count ['a';'l';'a'] 'a';;
count ['a';'l';'a'] 'l';;
count ['a';'l';'a'] 'o';;

let replicate (elem: 'a) (count: int) =
	if count < 0 
		then raise (Failure "Illegal argument exception: replication count cannot have negative value")
		else let rec replicateIt (c: int) (r: 'a list) =
			if c > 0
				then replicateIt (c - 1) (elem::r)
				else r
		in replicateIt count [];;
		
replicate "fhg" 10;;
replicate "fhg" 1;;
replicate "fhg" 0;;

let sqrList (list: int list) = 
	let rec sqrListInt (l: int list) (r: int list) =
		match l with
			[] -> List.rev r |
			h::t -> sqrListInt t ((h * h)::r)
	in sqrListInt list [];;
	
sqrList [1;2;3;-4];;
sqrList [];;

let isPalindrome (list: 'a list) = list = (List.rev list);;

isPalindrome [1;2;3;4;3;2;2];;
isPalindrome [1;2;3;4;3;2;1];;
isPalindrome [2;2;3;4;4;3;2;1];;
isPalindrome [1;2;3;4;4;3;2;1];;
isPalindrome [1;2;3;4;6;2;3;2;1];;
isPalindrome [1;2;3;4;6;4;3;2;1];;

let listLength (list: 'a list) = 
	let rec lenIt (l: 'a list) (r: int) =
		match l with
			[] -> r |
			_::t -> lenIt t (r+1)
	in lenIt list 0;;
	
listLength [1;2;3;4;5;6;7];;
listLength [1];;
listLength [];;

let swap (list: 'a list) (index: int) =
	let rec swapIt (l: 'a list) (i: int) (r: 'a list) =
		match l with
			[] -> List.rev r |
			h::t -> if i > 0 
				then swapIt t (i - 1) (h::r)
				else (l@(List.rev r))
	in swapIt list index [];;

swap [1;2;3;4;5] 0;;
swap [1;2;3;4;5] 1;;
swap [1;2;3;4;5] 2;;
swap [1;2;3;4;5] 3;;
swap [1;2;3;4;5] 4;;
swap [1;2;3;4;5] 5;;

let genList (startInt: int) (endInt: int) =
	let rec genListIt (c: int) (r: int list) =
		if c >= startInt
			then genListIt (c - 1) (c::r)
			else r
	in genListIt endInt [];;
	
genList 2 1;;
genList 0 0;;
genList 0 500;;
genList 0 1000000;;

let fibTail (index: int) =
	if index < 0 then raise (Failure "Illegal margument exception: Number of elements cannot be negative")
	else match index with
		0 -> 0 |
		1 -> 1 |
		_ -> let rec fibTailIt (i: int) (f: int) (s: int) =
				if i <= index 
					then fibTailIt (i + 1) s (s+f)
					else s
			in fibTailIt 2 0 1;;

fibTail 0;;
fibTail 1;;
fibTail 2;;
fibTail 10;;
fibTail 1000000;;

let rec fibNoTail (index: int) =
	if index < 0 then raise (Failure "Illegal margument exception: Number of elements cannot be negative")
	else match index with
		0 -> 0 |
		1 -> 1 |
		_ -> (fibNoTail(index - 1)) + (fibNoTail(index - 2));;
		
fibNoTail 0;;
fibNoTail 1;;
fibNoTail 2;;
fibNoTail 10;;

let root3 (value: float) (eps: float) =
	if eps >= 1.0 || eps <= 0.0 then raise (Failure "Illegal argument exception: The value of epylon has to be between 0 and 1")
	else let comparator = eps *. (abs_float value) in let rec root3It (x: float) =
		if (abs_float (x *. x *. x -. value)) <= comparator 
			then x
			else root3It (x +. (((value /. (x *. x)) -. x) /. 3.0))
	in if value >= 1.0
		then root3It (value /. 3.0)
		else root3It value;;
		
root3 8.0 0.000000000000001;;
root3 27.0 0.000000000000001;;
root3 0.125 0.000000000000001;;
root3 0.324113412 0.000000000000001;;

0.686908674659682306 *. 0.686908674659682306 *. 0.686908674659682306;;

let rec isSublist (sublist: 'a list) ( list: 'a list) =
	match (sublist, list) with
		([], _) -> true |
		(_, []) -> false |
		(sh::st, lh::lt) -> if sh = lh
			then isSublist st lt
			else false;;
			
isSublist [] ['a';'b';'c';'d'];;
isSublist [] [];;
isSublist ['a'] ['a';'b';'c';'d'];;
isSublist ['a';'b'] ['a';'b';'c';'d'];;
isSublist ['a';'b';'c';'d';'e'] ['a';'b';'c';'d'];;
isSublist ['a';'b';'c';'c'] ['a';'b';'c';'d'];;
isSublist [1] [];;
