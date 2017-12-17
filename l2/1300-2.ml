let rec removeAndCountIt (source: 'a list) (elem: 'a) (acc: int list) = 
match (source, acc) with 
([], _) -> List.rev acc |
(h::t, []) -> raise (Failure "result should not be empty") |
(sh::st, ah::at) -> if sh = elem then (removeAndCountIt st elem ((ah + 1)::at)) else (removeAndCountIt st sh (1::acc));;

let removeAndCount (source: 'a list) = if source = [] then [] else removeAndCountIt (List.tl source) (List.hd source) [1];;

removeAndCount [1;1;1;1;2;3;3;1;1;4;4];;
