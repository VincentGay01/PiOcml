

  let rec gouteagoutev2 (nombre:float) (it:float) (stop:float) =
    if it=stop then gouteagoutev2 (2. +. ((it +. 1.) /. (2. *. (it +. 1.) +. 1. ) ) ) (stop -. 1.) stop 
    else if it=1. then nombre 
      else 
        gouteagoutev2 ( 2. +. (((it -. 1. ) /. (2. *. (it -. 1. ) +. 1. )) *. nombre))  (it -. 1. ) stop  ;;


let algoGoute stop =
  gouteagoutev2 (0.) (stop) stop ;;

let pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679 ;;

let x= algoGoute 1000. ;;
let x= algoGoute 100. ;;
let x= algoGoute 50. ;;
let x= algoGoute 25. ;;