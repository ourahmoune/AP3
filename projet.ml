open List ;;
#load "btreeS.cmo" ;;
open BtreeS ;;
#load "bst.cmo" ;; 
open Bst ;;
open Random ;;
#load "avl.cmo";;
open Avl ;;

let rec exicte_in (e ,l : 'a * 'a list) : bool = 
    if(l = []) then 
        false
    else
        if(hd(l)==e) then 
            true
        else
            exicte_in(e,tl(l))
;;


let rec generer_liste_taille_aux (taille : int ) : 'a list =
  if taille <= 0 then
    []
  else
    if(taille >= 2048*2048*256)  then
      failwith "la borne max de Random est 2^30 on peut construire une liste de la taille plus de 2^30 avec des valeure distinctes"
    else
      if(taille>10000) then
        failwith "on suppose que la taille de la liste inf ou egale a 10000 pour bien voire"
      else
        let element : int ref=ref  (Random.int 10000)  in
        let reste_liste = generer_liste_taille_aux(taille  -1) in
        while (exicte_in(!element,reste_liste))
        do
          element :=  Random.int 10000 ;(*la valeure max de Random.int*)
        done;      
        !element::reste_liste
    
;;
let generate_liste_taille(taille:int):'a list=
  let () = Random.self_init() in
  generer_liste_taille_aux(taille)
;;

let   bst_rnd_create(l:'a list) : 'a btree =
  bst_lbuild(l)
;;

(*Qst3*)

let list_sort( l : 'a list ) : 'a list  =
  List.sort compare l 
;;

let rec sub_list (l , t : 'a list * int ) :'a list *'a list  =
  if (l == [] ) then
    ([],[])
  else
    if(t==0 ) then
      (l,[])
    else
      let fst = hd(l) and
          rst = tl(l) in
      let (restlist , res) = sub_list(rst, t-1 ) in
      (restlist , fst::res)
;;


let rec decoupe_aux (l , taille : 'a list * int ) : 'a list    =
  if(l ==[]) then
    []
  else
    let  soustaille: int  ref=ref ( Random.int taille-1) in
    while(! soustaille > taille)do
      soustaille := Random.int taille-1 ;
    done;
    
    let (reste , souslist) = sub_list(l, !soustaille) in
    let liste_sort = list_sort(souslist) in 
    liste_sort@decoupe_aux(reste , taille- !soustaille )
;;

let decoupe(l,taille : 'a list * int ) : 'a list  =
  let () = Random.self_init() in
  decoupe_aux(l,taille)

;;
let avl_rnd_create( l  :'a list ) : 'a t_avl =
  avl_lbuild(l)
;;

 
