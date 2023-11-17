open List ;;
#load "btreeS.cmo" ;;
open BtreeS ;;
#load "bst.cmo" ;; 
open Bst ;;
open Random
let rec exicte_in (e ,l : 'a * 'a list) : bool = 
    if(l = []) then 
        false
    else
        if(hd(l)==e) then 
            true
        else
            exicte_in(e,tl(l))
;;


   
let rec generer_liste_taille (taille : int ) : 'a list =
  if taille <= 0 then
    []
  else
    let element = Random.int 1024*16 in 
    let reste_liste = generer_liste_taille (taille - 1) in
    if exicte_in (element , reste_liste) then
      reste_liste
    else
      element :: reste_liste
;;

let   bst_rnd_create(taille : int ) : 'a btree = 
  let () = Random.self_init() in
  let list =  generer_liste_taille(taille) in
  bst_lbuild(list)
;;




