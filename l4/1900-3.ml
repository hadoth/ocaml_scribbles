let trapezoid (inputF: float -> float) (startR: float) (endR: float) =
	(endR -. startR) *. ((inputF endR) +. (inputF startR)) /. 2.0;;

let rec integralIt (inputF: float -> float) (startR:float) (endR: float) (parts: int) (acc: float) =
if parts = 0 
	then acc 
	else 
		integralIt 
			inputF 
			(((endR -. startR) /. (float_of_int parts)) +. startR) 
			endR 
			(parts - 1) 
			(acc +. (trapezoid inputF startR (startR +. ((endR -. startR) /. (float_of_int parts)))));;
	
let integral (inputF: float -> float) (ends: float * float) (parts: int) =
	let (startR, endR) = ends in integralIt inputF startR endR parts 0.0;;
	
let linear (x: float) = 5.0 *. x;;
let quadratic (x:float) = (2.0 *. x *. x) -. (8.0 *. x) +. 3.5;;

trapezoid quadratic 0.0 1.1;;
trapezoid quadratic 1.0 2.1;;
trapezoid quadratic 2.0 3.1;;
trapezoid quadratic 3.0 4.1;;
trapezoid quadratic 4.0 5.1;;

integral linear (0.0, 5.0) 1;;
integral quadratic (0.0, 5.0) 1;;
integral quadratic (0.0, 5.0) 2;;
integral quadratic (0.0, 5.0) 3;;
integral quadratic (0.0, 5.0) 4;;
integral quadratic (0.0, 5.0) 5;;
integral quadratic (0.0, 5.0) 6;;
integral quadratic (0.0, 5.0) 7;;
integral quadratic (0.0, 5.0) 8;;
integral quadratic (0.0, 5.0) 9;;
integral quadratic (0.0, 5.0) 10;;
integral quadratic (0.0, 5.0) 20;;
integral quadratic (0.0, 5.0) 30;;
integral quadratic (0.0, 5.0) 40;;
integral quadratic (0.0, 5.0) 50;;
integral quadratic (0.0, 5.0) 75;;
integral quadratic (0.0, 5.0) 100;;
integral quadratic (0.0, 5.0) 500;;
integral quadratic (0.0, 5.0) 1000;;

