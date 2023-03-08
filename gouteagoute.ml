 (*let rec  gouteagoute (nombre:float) (it:float) (stop:float) =
 if it=0.  then  gouteagoute (2. +. (1. /. 3. )) 1. stop 
 else if it=stop then  nombre
 else
  gouteagoute ( 2. +. ((it +. 1. ) /. (2. *. (it +. 1. ) +. 1. )) *. nombre)  (it +. 1. ) stop  ;;*)

  let rec gouteagoutev2 (nombre:float) (it:float) (stop:float) =
    if it=stop then gouteagoutev2 (2. +. ((it +. 1.) /. (2. *. (it +. 1.) +. 1. ) ) ) (stop -. 1.) stop 
    else if it=1. then nombre 
      else 
        gouteagoutev2 ( 2. +. (((it -. 1. ) /. (2. *. (it -. 1. ) +. 1. )) *. nombre))  (it -. 1. ) stop  ;;


let AlgoGoute stop =
  gouteagoutev2 (0.) (stop) stop ;;



let x= AlgoGoute 1000. ;;
let x= AlgoGoute 100. ;;
let x= AlgoGoute 50. ;;
let x= AlgoGoute 25. ;;