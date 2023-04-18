let rec bbp (nombre:float) (it:float) (stop:float) =
if it =0. then bbp (((1. /. (16. ** 0. )  ) *. (( 4. /. ((8. *. 0. ) +. 1.) ) -. (2. /. ((8. *. 0. ) +. 4.))-. (1. /. ((8. *. 0. ) +. 5.))-. (1. /. ((8. *. 0. ) +. 6.)))  ) ) (1.) stop 
else if it=stop then nombre
  else 
    bbp (nombre +. ((1. /. (16. ** it ) ) *. (( 4. /. ((8. *. (it ) ) +. 1. ) ) -. (2. /. ((8. *. (it ) ) +. 4.))-. (1. /. ((8. *. (it ) ) +. 5.))-. (1. /. ((8. *. (it ) ) +. 6.)))  ) ) (it +. 1.) stop ;;


let auxbbp stop =
  bbp 0. 0. stop ;;

  
let plop = auxbbp 1. ;;
let plop = auxbbp 2. ;;
let plop = auxbbp 3. ;;
let plop = auxbbp 4. ;;
let plop = auxbbp 5. ;;
let plop = auxbbp 1000000. ;;