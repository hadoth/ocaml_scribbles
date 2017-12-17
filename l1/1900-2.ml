let rec remove (list: 'a list) (toRemove: int) = if toRemove < 0 then raise (Failure "Number of elements to delete cannot be lower than 0") else if toRemove = 0 || list = [] then list else remove (List.tl list) (toRemove - 1);;

remove [5;4;3;2;1] 0;;
remove [5;4;3;2;1] 1;;
remove [5;4;3;2;1] 2;;
remove [5;4;3;2;1] 3;;
remove [5;4;3;2;1] 4;;
remove [5;4;3;2;1] 5;;
