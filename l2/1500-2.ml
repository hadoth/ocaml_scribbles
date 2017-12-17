let rec reverseIt  (list: 'a list) (rev: 'a list) = match list with [] -> rev | h::t -> reverseIt t (h::rev);;

let reverse (list: 'a list) = reverseIt list [];;

reverse [5;4;3;2];;
