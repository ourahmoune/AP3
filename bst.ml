open BtreeS ;;

let rec bst_seek (t , e : 'a btree * 'a ) : bool =
    if (bt_isempty(t)) then 
        false
    else
        let r = bt_root(t) and 
            fg = bt_subleft(t) and 
            fd = bt_subright(t) in 
        if (r=e) then 
            true 
        else
            if (e>r) then 
                bst_seek(fd,e)
            else
                bst_seek(fg,e)
;;


let rec bst_insert (t , e : 'a btree * 'a) : 'a btree = 
    if(bt_isempty(t)) then 
        bt_rooting(e,bt_empty(),bt_empty())
    else
        let r = bt_root(t) and 
            fg = bt_subleft(t) and 
            fd = bt_subright(t) in
        if(e>r) then 
            bt_rooting(r,fg , bst_insert(fd,e))
        else
            if(e<r) then 
                bt_rooting(r,bst_insert(fg,e) , fd)
            else
                failwith "error : il faut pas insérer des elements qui exictent deja dans un abr"
;;


let rec bst_lbuild (l:'a list ) : 'a btree = 
    match l with 
    []-> bt_empty() 
    |
    e::r -> bst_insert(bst_lbuild(r), e)
;;


let rec bst_max (t:'a btree) : 'a = 
    if bt_isempty(t) then 
        failwith"error -> bst_max est partielle : on peut pas chercher un elements dans un abr vide  "
    else
        let fd = bt_subright(t) in 
        if bt_isempty(fd) then 
            bt_root(t)
        else    
            bst_max(fd)
;;


let rec bst_deletemax (t:'a btree) : 'a btree =
    if (bt_isempty(t)) then 
        failwith"error-> bst_deletemax est partielle : on peut pas supprimer un élements dans un abr vide "
    else
        let fd = bt_subright(t) in 
        if (bt_isempty(fd)) then 
            bt_subleft(t)
        else
            bt_rooting(bt_root(t) ,bt_subleft(t) , bst_deletemax(fd) )
;;

let rec  bst_delete (t,e : 'a btree* 'a):'a btree = 
    if(bt_isempty(t)) then 
        t
    else
        let fg  = bt_subleft(t) and 
            fd = bt_subright(t) and 
            r = bt_root(t)in 
        if (e>r) then 
            bt_rooting(r,fg, bst_delete(fd,e))
        else
            if(e<r) then
                bt_rooting(r,bst_delete(fg,e), fd)
            else (*si on veut sup la racine  e=r *)

            if(not(bt_isempty(fg)) &&  not(bt_isempty(fd)) ) then  (*fd et fg ne sont pas vide*) 
                let max = bst_max(fg) and 
                    fg = bst_deletemax(fg) in 
                bt_rooting(max,fg,fd)

            else (* soit fg est vide soit fd est vide soit les deux sont vide   *)
                if(not(bt_isempty(fg)))  then (*donc fd pas vide  *)
                    fg
                else(*fg est vide *)
                    if(not(bt_isempty(fd)))then (*fg vide fd non vide *)
                        fd
                    else(*fg et fd sont vide *)
                        bt_empty()
;;






 
    

