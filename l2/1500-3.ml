let rec ends (list: 'a list) = match list with 
[] -> raise (Failure "Cannot call on empty list") |
h::[] -> (h,h) |
f::s::[] -> (f,s) |
f::s::t -> ends (f::t);;

ends [1;2;3;4;5;6;7];;
ends [1];;
ends [];;
