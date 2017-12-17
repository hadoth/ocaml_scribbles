let hasFirst list = list <> [];;
let hasSecond list = hasFirst (List.tl list);;
let hasTwo list = hasFirst list && hasSecond list;;

let rec isSortedAsc list = 
	if (hasTwo list) 
		then (isSortedAsc (List.tl list) && List.hd list <= List.hd (List.tl list))
		else true
;;

let rec isSortedDesc list =
	if (hasTwo list)
		then (isSortedDesc (List.tl list) && List.hd list >= List.hd (List.tl list))
		else true
;;

let isSorted list = isSortedAsc list || isSortedDesc list;;

let sorted1 = [1;2;3;4;5];;
let sorted2 = [1,1,1,1,1];;
let sorted3 = [1,1,1,1,5];;
let sorted4 = [1,5,5,5,5];;
let sorted5 = [5;4;3;2;1];;
let sorted6 = ["a","b","c","d","e"];;
let sorted7 = [5,1,1,1,1];;
let sorted8 = [5,5,5,5,1];;
let sorted9 = [1];;
let sorted10 = [];;

let notSorted1 = [1;2;3;2;1];;
let notSorted2 = [1;5;5;5;1];;
let notSorted3 = [5;1;1;1;5];;
let notSorted4 = [1;2;3;4;1];;
let notSorted5 = [5;4;3;2;5];;

isSorted sorted1;;
isSorted sorted2;;
isSorted sorted3;;
isSorted sorted4;;
isSorted sorted5;;
isSorted sorted6;;
isSorted sorted7;;
isSorted sorted8;;
isSorted sorted9;;
isSorted sorted10;;

isSorted notSorted1;;
isSorted notSorted2;;
isSorted notSorted3;;
isSorted notSorted4;;
isSorted notSorted5;;
