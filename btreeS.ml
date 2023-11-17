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
