let rec counEvensIt (list: int list) (acc: int) = match list with [] -> acc | h::t -> if (h mod 2 == 0) then counEvensIt t (acc + 1) else counEvensIt t acc;;

let countEvens (list: int list) = counEvensIt list 0;;

countEvens [5;4;3;2];;
