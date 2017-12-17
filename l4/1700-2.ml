let rec isSublistIt (list: 'a list) (sublist: 'a list) (sublistFragment: 'a list)=
match (list, sublistFragment) with 
	(_, []) -> true |
	([], _) -> false |
	(lh::lt, sh::st) -> if lh = sh 
		then isSublistIt lt sublist st
		else isSublistIt lt sublist sublist;;
		
let isSublist (sublist: 'a list) (list: 'a list) = isSublistIt list sublist sublist;;

isSublist [11] [11;22];;
isSublist [11;22] [0;11;22;33];;
isSublist [0;11;22] [0;11;22;33];;
isSublist [0;22;11] [0;11;22;33];;
isSublist [0;22;33] [0;11;22;33];;
