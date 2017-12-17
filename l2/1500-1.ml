let rec lenIt (list: 'a list) (size: int) = match list with [] -> size | _::t -> lenIt t (size + 1);;

let len (list: 'a list) = lenIt list 0;;

len [1;2;3];;
len [1];;
len [];;
