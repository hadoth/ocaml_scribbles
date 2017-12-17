let rec nth (list: 'a list) (index: int) = if index < 1 || list = [] then raise (Failure "Index out of bounds") else if index = 1 then List.hd list else nth (List.tl list) (index - 1);;

nth [1;2;3;4;5;6;7;8;9;10] 1;;
nth [1;2;3;4;5;6;7;8;9;10] 5;;
nth [1;2;3;4;5;6;7;8;9;10] 10;;

let rec divideIt (acc: 'a list) (list: 'a list) (index: int) = if index < 0 || (index > 0 && list = []) then raise (Failure "Index out of bounds") else if index = 0 then (List.rev acc, list) else divideIt ((List.hd list)::acc) (List.tl list) (index - 1);;

let divide (list: 'a list) (index: int) = divideIt [] list index;;


divide [1;2;3;4;5] 0;;
divide [1;2;3;4;5] 1;;
divide [1;2;3;4;5] 2;;
divide [1;2;3;4;5] 3;;
divide [1;2;3;4;5] 4;;
divide [1;2;3;4;5] 5;;
