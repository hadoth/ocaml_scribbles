let rec drop (list, dropCount) = if dropCount < 0 then raise (Failure "Illegal cut argument!") else if dropCount = 0 then list else drop (List.tl list, dropCount - 1);;

let list5 = [1;2;3;4;5];;
let list1 = [1];;
let list0 = [];;

list5;;
drop (list5, 2);;
drop (list5, 0);;
drop (list5, -1);;
drop (list5, 10);;
drop (list5, 5);;

list1;;
drop (list1, 1);;
drop (list1, 0);;
drop (list1, -1);;
drop (list1, 2);;

list0;;
drop (list0, 1);;
drop (list0, 0);;
drop (list0, -1);;
