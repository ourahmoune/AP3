open List ;;
#load "btreeS.cmo" ;;
open BtreeS ;;
#load "bst.cmo" ;; 
open Bst ;;
open Random ;;

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
      
      let element : int ref=ref  (Random.int taille-1)  in 
      let reste_liste = generer_liste_taille_aux(taille - 1) in
      while (exicte_in(!element,reste_liste))
      do
        element :=  Random.int 2048*2048*256-1 ;(*la valeure max de Random.int*)
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


let  desiquilibre_moyen (t,taille : 'a btree * int)  : float  =
  float_of_int(desiquilibre_total(t) ) /. float_of_int(taille) 
;;


(*on remarque que avec des nombre al�atoire 
la moyen de d�siquilibre est trop petit
*)


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
    

let rec taille (t: 'a btree ) : int =
  if(bt_isempty(t)) then 0
  else
    1 + taille(bt_subleft(t))+ taille(bt_subright(t))
;;

let rec taille_list(l:'a list) : int  =
  match l with
    []->0
  |
    fst::r -> 1+taille_list(r)
;;

 
let list = generer_liste_taille(2000) ;;
let listdecope = decoupe (list,2000 ) ;;
taille_list(listdecope) ;;
desiquilibre_moyen(bst_rnd_create(list),2000);;
desiquilibre_moyen(bst_rnd_create(listdecope),2000) ;;


