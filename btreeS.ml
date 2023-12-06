type 'a btree  = EMPTY | ROOTING of 'a * 'a btree * 'a btree  ;; 
let bt_empty ():'a btree =
  EMPTY 
;;

let bt_rooting (x,fg,fd : 'a*'a btree *'a btree ) : 'a btree =
    ROOTING(x,fg,fd)
    
;;
let bt_empty ():'a btree =
  EMPTY
;;

let bt_isempty (t: 'a btree) : bool =
   match t with 
   EMPTY -> true
   |
   _ -> false
;;

let bt_root (t : 'a btree ) : 'a=
match t with 
EMPTY -> failwith "ton arbre est vide "
|
ROOTING(x,_,_) -> x 
;;


let bt_subleft (t:'a btree): 'a btree = 
match t with 
EMPTY -> failwith "l'arbre est vide "
|
ROOTING(_,fg,_) -> fg 
;;
let bt_subright (t:'a btree): 'a btree = 
match t with 
EMPTY -> failwith "l'arbre est vide "
|
ROOTING(_,_,fd) -> fd ;;

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
     -1 - hateur(fd)
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
    abs(desiquilibre(t)) + desiquilibre_total(bt_subleft(t))+ desiquilibre_total(bt_subright(t))
;;


let  desiquilibre_moyen (t,taille : 'a btree * int)  : float  =
  float_of_int(desiquilibre_total(t) ) /. float_of_int(taille) 
;;
let max (a,b :'a*'a) :'a =
  if (a>b) then 
    a
  else
    b
;;
