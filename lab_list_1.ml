let funA (x: int) (y:int) = x>y;;

funA 10 5;;
funA 5 10;;
funA 5 5;;

let funB (x: float) (y:float) = if x > y then (y,x) else (x,y);;

funB 2.0 5.0;;
funB 5.0 2.0;;
funB 2.0 2.0;;

let rec funC (x: 'a list) (y:int) = if y <= 0 || x = [] then x else funC (List.tl x) (y - 1);;

funC ['a'; 'b'; 'c'] 2;;
funC ['a'; 'b'; 'c'] 0;;
funC ['a'; 'b'; 'c'] 4;;

let rec twoLast list: 'a list = if (list <> [] && List.tl list <> [] && List.tl (List.tl list) <> []) then twoLast (List.tl list) else list;;

twoLast ['a'; 'b'; 'c'; 'd'];;
twoLast ['a'; 'b'; 'c'];;
twoLast ['a'; 'b'];;
twoLast ['a'];;
twoLast [];;

let rec twoEqual (list: 'a list) = match list with [] -> false | h::[] -> false | f::s::t -> if f = s then true else (twoEqual (s::t));;

twoEqual [1;2;3;4;5];;
twoEqual [1;1;2;3;4;5];;
twoEqual [1;2;3;3;4;5];;
twoEqual [1;2;3;4;5;5];;
twoEqual [1;2;1;3;1;4;1;5;1];;
	
