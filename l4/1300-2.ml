let rec codeList (input: 'a list)  (output: (int * 'a) list) (elem: 'a) (acc: int) =
	match input with 
		[] -> List.rev ((acc, elem)::output) |
		h::t -> 
			if h = elem 
				then codeList t output elem (acc + 1) 
				else codeList t ((acc, elem)::output) h 1;;
				
let code (list: 'a list) = match list with [] -> [] | h::t -> codeList t [] h 1;;

code ['a';'a';'a';'b';'b';'b';'b';'c'];;
