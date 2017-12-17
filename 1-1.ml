let hasFirst list = list <> [];;
let hasSecond list = hasFirst (List.tl list);;
let hasTwo list = hasFirst list && hasSecond list;;

let listStart list = List.hd list;;
let listEnd list = listStart (List.rev list);;

let ends list = if (hasTwo list) then (listStart list, listEnd list) else raise (Failure "List is too short");;

let test1 = [1;2;3;4;5];;
let test2 = [1;2];;
let test3 = ["a";"b";"c";"d";"e"];;
let test4 = [1];;
let test5 = [];;

ends test1;;
ends test2;;
ends test3;;
ends test4;;
ends test5;;
