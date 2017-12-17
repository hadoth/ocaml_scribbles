let rec wydajReszteIt cash input output = match (cash > 0, input) with (false, _) -> List.rev output | (_, h::t) -> if h <= cash then wydajReszteIt (cash - h) input (h::output) else wydajReszteIt cash t output | (_, []) -> raise (Failure "Nie można wydać reszty używając podanych nominałów");;

let wydajReszte cash input = wydajReszteIt cash input [];;
wydajReszte 69 [50;20;10;5;2;1];;
