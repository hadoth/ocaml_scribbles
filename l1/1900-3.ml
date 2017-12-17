let doubleVal (x: float) = 2.0 *. x;;

let power (x: float) = x *. x;;

let rec processIt (list: float list) (result: float list) (processor: float -> float) = if list = [] then List.rev result else processIt (List.tl list) ((processor (List.hd list))::result) processor;;

let process (list: float list) (processor: float -> float) = processIt list [] processor;;

process [5.; 3.; 2.] doubleVal;;
process [5.; 3.; 2.] power;;
