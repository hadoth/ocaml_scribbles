let isNotGrowing (pair: float * float * float) = let (r1, r2, r3) = pair in r1 >= r2 || r2>= r3;;

isNotGrowing (3.,2.,4.);;
isNotGrowing (2.,3.,4.);;
