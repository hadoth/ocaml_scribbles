let rec joinIt (list: string list) (separator: string) (acc: string) =  match list with
	[] -> acc |
	h::[] -> acc^h |
	h::t -> joinIt (t) separator (acc^(h^separator));;
	
let join (list:string list) (separator: string) = joinIt list separator "";;

joinIt ["To"] "-" "";;
joinIt ["To"; "jest"] "-" "";;
joinIt ["To"; "jest"; "napis"] "-" "";;

join ["To"; "jest"; "napis"] "-";;
