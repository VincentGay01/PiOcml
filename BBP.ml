let rec BBP (nombre:float) (it:float) (stop:float) =
if it =0. then BBP (((1. /. (16. ** 0. )  ) *. (( 4. /. ((8. *. 0. ) +. 1.) ) -. (2. /. ((8. *. 0. ) +. 4.))-. (1. /. ((8. *. 0. ) +. 5.))-. (1. /. ((8. *. 0. ) +. 6.)))  ) ) (1.) stop 
else if it=stop then nombre
  else 
    BBP (nombre +. ((1. /. (16. ** it +. 1.) ) *. (( 4. /. ((8. *. (it +. 1.) ) +. 1. ) ) -. (2. /. ((8. *. (it +. 1.) ) +. 4.))-. (1. /. ((8. *. (it +. 1.) ) +. 5.))-. (1. /. ((8. *. (it +. 1.) ) +. 6.)))  ) ) (it +. 1.) stop ;;


let auxBBP stop =
  BBP 0. 0. stop ;;

  
let plop = auxBBP 1. ;;
let plop = auxBBP 2. ;;
let plop = auxBBP 3. ;;
let plop = auxBBP 4. ;;
let plop = auxBBP 5. ;;
let plop = auxBBP 1000000. ;;
