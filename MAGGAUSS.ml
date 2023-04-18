let rec maggauss  (ak:float) (bk:float) (sk:float) (k:float) (stop:float)=
   let t = ak in
   let akp1 = ((ak +. bk) /. (2.)) in
   let bkp1 = ((t *. bk) ** (1. /. 2.)) in
   let ck = ((akp1 -. t ) ** 2. ) in
   let skp1= (sk -. (2. ** (k)) *. ck ) in 
  if k=0. then maggauss ( 1. ) ( 1. /. (2. ** (1. /. 2.)) ) (1. /. 2.) (1.) (stop)
  else if k= stop  then (((ak +. bk) ** 2. ) /. (2. *. sk))
  else 
    maggauss (akp1) (bkp1) (skp1) (k +. 1.) (stop) 
  ;;

  let Auxmaggauss stop =
    maggauss 0. 0. 0. 0. stop ;;

  let test = Auxmaggauss 5. ;;
  let test = Auxmaggauss 10. ;;


