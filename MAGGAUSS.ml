let rec MAGGAUSS  (ak:float) (bk:float) (sk:float) (k:float) (stop:float)=
   let t = ak in
   let akp1 = ((ak +. bk) /. (2.)) in
   let bkp1 = ((t *. bk) ** (1. /. 2.)) in
   let ck = ((akp1 -. t ) ** 2. ) in
   let skp1= (sk -. (2. ** (k)) *. ck ) in 
  if k=0. then MAGGAUSS ( 1. ) ( 1. /. (2. ** (1. /. 2.)) ) (1. /. 2.) (1.) (stop)
  else if k= stop  then (((ak +. bk) ** 2. ) /. (2. *. sk))
  else 
    MAGGAUSS (akp1) (bkp1) (skp1) (k +. 1.) (stop) 
  ;;

  let AuxMAGGAUSS stop =
    MAGGAUSS 0. 0. 0. 0. stop ;;

  let test = AuxMAGGAUSS 5. ;;
  let test = AuxMAGGAUSS 10. ;;


