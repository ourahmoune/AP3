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

(*hateur *)
let max (a,b :'a*'a) :'a =
  if (a>b) then 
    a
  else
    b
;;

let rec hateur( t : 'a btree) :int  =
  if (bt_isempty(t)) then 
    failwith"error : faonction hateur est partielle il faut pas l'appeler sur arbre vide  "
  else
  let fg = bt_subleft(t) and 
      fd  = bt_subright(t)in 
  if(bt_isempty(fg) && bt_isempty(fd))then
    0
  else (*soit fg n'est pas vide soit fd n'est pas vide soit les deux ne sont pas vide *)
    if(bt_isempty(fg) )then(*fg est vide donc fd n'est pas vide*)
      1 + hateur(fd)
    else(*fg n'est pas vide *)
      if(bt_isempty(fd)) then 
        1+hateur(fg)
      else(*fg fg n'est pas vide et fd  non plus*)
        1+ max(hateur(fg),hateur(fd))
;;

let desiquilibre (t:'a btree) : int  = 
  if(bt_isempty(t)) then 
    0
  else
  let fg = bt_subleft(t) and 
      fd  = bt_subright(t)in 
  if(bt_isempty(fg) && bt_isempty(fd))then
    0
  else (*soit fg n'est pas vide soit fd n'est pas vide soit les deux ne sont pas vide *)
    if(bt_isempty(fg) )then(*fg est vide donc fd n'est pas vide*)
      hateur(fd)+1
    else(*fg n'est pas vide *)
      if(bt_isempty(fd)) then 
        1+hateur(fg)
      else(*fg fg n'est pas vide et fd  non plus*)
        hateur(fg)-hateur(fd)
;;
let rec  desiquilibre_total(t:'a btree) :int  =
  if(bt_isempty(t)) then 
    0
  else  
    desiquilibre(t) + desiquilibre_total(bt_subleft(t))+ desiquilibre_total(bt_subright(t))
;;


let  desiquilibre_moyen (taille : int)  : float  =
  let t = bst_rnd_create(taille) in
  float_of_int(desiquilibre_total(t) ) /. float_of_int(taille) 
;;
desiquilibre_moyen(439) ;;
(*on remarque que avec des nombre aléatoire 
la moyen de désiquilibre est trop petit
*)





