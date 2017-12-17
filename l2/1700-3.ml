let rec switchIt (left: 'a list) (right: 'a list) (counter: int) =
if counter = 0 then (left@(List.rev right)) else
	match left with 
		[] -> raise (Failure "Index out of bounds!") |
		h::t -> switchIt t (h::right) (counter - 1);;
		
let switch (list: 'a list) (counter: int) = switchIt list [] counter;;

switch [4;5;6;7] 0;;
switch [4;5;6;7] 1;;
switch [4;5;6;7] 2;;
switch [4;5;6;7] 3;;
switch [4;5;6;7] 4;;
