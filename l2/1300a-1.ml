let rec divideIt (list: int list) (evens: int list) (odds: int list) =
match list with 
[] -> (List.rev evens, List.rev odds) |
h::t -> if (h mod 2) = 0 then divideIt t ((h * h)::evens) odds else divideIt t evens ((3 * h)::odds);;

let divide (list: int list) = divideIt list [] [];;

divide [3;6;8;9;13];;
