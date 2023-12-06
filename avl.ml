
open BtreeS ;;
type 'a t_avl = ('a * int ) btree ;;
open Bst ;;
open List ;;




let ab_rg (a: 'a t_avl ): 'a t_avl  = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rg non definie sur les arbre vide "
    else
        let fd = bt_subright(a) in 
        if(bt_isempty(fd)) then 
            failwith"error : ab_rg est non definie sur les arbre qui n'ont pas de fils droite"
        else    
            let (p,dp) = bt_root(a) and 
                u = bt_subleft(a) and 
                (q,dq)= bt_root(fd) and 
                v= bt_subleft(fd) and 
                w= bt_subright(fd) in 
            bt_rooting((q,0),bt_rooting((p,0),u,v),w)
;;

let ab_rd (a: 'a t_avl ): 'a t_avl = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rg non definie sur les arbre vide "
    else
        let fg = bt_subleft(a) in 
        if(bt_isempty(fg)) then 
            failwith"error : ab_rd est non definie sur les arbre qui n'ont pas de fils gauche"
        else    
            let (q,dq) = bt_root(a) and 
                w = bt_subright(a) and 
                (p,dp)= bt_root(fg) and 
                u= bt_subleft(fg) and 
                v= bt_subright(fg) in 
            bt_rooting((p,0),u,bt_rooting((q,0),v,w))
;;

let ab_rgd (a: 'a t_avl ): 'a t_avl = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rgd non definie sur les arbre vide "
    else
        let fg = bt_subleft(a) in 
        if(bt_isempty(fg)) then 
            failwith"error :ab_rgd  est non definiÃ© sur les arbre qui n'ont pas de fils gauche "
        else
            let fdg = bt_subright(fg) in 
            if( bt_isempty(fdg)) then 
                failwith"error : ab_rgd est non definiÃ© sur les arbre qui n'ont pas un fils droite de fils gauche"
            else
                let (r,dr) = bt_root(a) and 
                    (p,dp) = bt_root(fg) and 
                    t= bt_subleft(fg) and
                    (q,dq)= bt_root(fdg) and 
                    u=bt_subleft(fdg) and 
                    v=bt_subright(fdg)and 
                    w=bt_subright(a) in 
                bt_rooting((q,0),bt_rooting((p,0),t,u) ,bt_rooting((r,0),v,w))

;;

let ab_rdg (a: 'a t_avl ): 'a t_avl = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rdg non definie sur les arbre vide "
    else
        let fd = bt_subright(a) in 
        if(bt_isempty(fd)) then 
            failwith"error :ab_rdg  est non definiÃ© sur les arbre qui n'ont pas de fils droite "
        else
            let fgd = bt_subleft(fd) in 
            if( bt_isempty(fgd)) then 
                failwith"error : ab_rdg est non definiÃ© sur les arbre qui n'ont pas un fils gauche de fils droite"
            else
                let (r,dr) = bt_root(a) and 
                    (p,dp)= bt_root(fd) and 
                    t= bt_subleft(a) and
                    (q,dq)= bt_root(fgd) and 
                    u=bt_subleft(fgd) and 
                    v=bt_subright(fgd)and 
                    w=bt_subright(fd) in 
                bt_rooting((q,0),bt_rooting((r,0),t,u) ,bt_rooting((p,0),v,w))

;;

let  ab_reequilibre (a: 'a t_avl) : 'a t_avl  =
  let (r,d) = bt_root(a) in
  if(d =1 || d = -1 || d=0) then
    a
  else
    
    if (d=2) then
        let (rg,dg) = bt_root(bt_subleft(a)) in 
        if(dg =1) then 
            ab_rd(a) 
        else
            if(dg= -1) then 
                ab_rgd(a)
            else
              failwith "error ----------- 1"
    else
        if(d= -2) then 
            let (rd,dd) = bt_root(bt_subright(a)) in 
        if(dd =1) then 
            ab_rdg(a)
        else
            if(dd = -1) then 
              ab_rg(a)
               else
              failwith "error : ----- 2"
        else
            failwith"error---- 3"
;;

let rec avl_ajt (a , e : 'a t_avl * 'a ) : 'a t_avl  =
  if(bt_isempty(a)) then
    bt_rooting((e,0),bt_empty(),bt_empty())
  else
    let (r,d) = bt_root(a) in
    if (e<r ) then
      let  fg =  avl_ajt(bt_subleft(a),e) in
      let new_avl = bt_rooting((r,d),fg,bt_subright(a)) in
      ab_reequilibre(bt_rooting(  (r,desiquilibre(new_avl)) ,fg,bt_subright(a))) 
    else
      if(e>r) then
        let fd = avl_ajt(bt_subright(a),e) in
        let new_avl = bt_rooting((r,d),bt_subleft(a), fd) in 
        ab_reequilibre(bt_rooting((r,desiquilibre(new_avl)),bt_subleft(a),fd))
      else
        a
;;
let avl_seek(a,e : 'a t_avl * 'a) : bool =
  bst_seek(a,(e,0)) || bst_seek(a,(e,1)) || bst_seek(a,(e,-1) )
;;


let rec avl_max (a : 'a t_avl ) : 'a * int  =
  if bt_isempty(a) then
    failwith "error -> avl_max est partielle : on peut pas chercher un elements dans un abr vide  "
  else
    let fd = bt_subright(a) in
    if bt_isempty(fd) then
      bt_root(a)
    else
      avl_max(fd)
    
;;

let rec avl_sup_max (a: 'a t_avl) : 'a t_avl =
  if bt_isempty(a) then
    failwith "eroor -> avl_sup_max est partielle : no peut pas supprimer un élément dans un avl vide"
  else
    let fd = bt_subright(a) in
    if bt_isempty(fd) then
      bt_subleft(a)
    else
      let (r,d) = bt_root(a) in 
      let fd = avl_sup_max(fd) in
      let new_avl = bt_rooting((r,d) , bt_subleft(a) , fd) in
      ab_reequilibre(      bt_rooting(    (r,desiquilibre(new_avl) ),bt_subleft(a),fd )  )
;;

let rec avl_sup (a,e: 'a t_avl * 'a) : 'a t_avl =
  if  bt_isempty(a) then
    a
  else
    let fg = bt_subleft(a) and
        fd = bt_subright(a) and
        (r,d) = bt_root(a) in
    if(e<r) then
      let fg = avl_sup(fg,e) in
      let new_avl = bt_rooting((r,d) ,fg, fd ) in
      ab_reequilibre(bt_rooting((r,desiquilibre(new_avl)),fg,fd ))
    else
      if(e>r) then
        let fd = avl_sup(fd,e) in
        let new_avl = bt_rooting((r,d) ,fg, fd ) in
        ab_reequilibre(bt_rooting((r,desiquilibre(new_avl)),fg,fd ))
      else(*si on veut supprimer la racine *)
        if (not(bt_isempty(fg)) && not(bt_isempty(fd))) then (*fd et fg ne sont pas vide*)
          let (max,dmax) = avl_max(fg) and
              fg = avl_sup_max(fg) in
          let new_avl = bt_rooting((max,dmax) , fg , fd) in
          ab_reequilibre(bt_rooting( (max,desiquilibre(new_avl)),fg,fd ))
        else(*soit fg est vide soit fd est vide soit les deux sont vide *)
          if(not(bt_isempty(fg))) then (*donc fd est vide*)
            fg
          else (* fg est vide  *)
            if(not(bt_isempty(fd))) then (* fg vide et fd non vide*)
              fd
            else(*fg et fd sont vide*)
              bt_empty()
;;




let rec  avl_lbuild(l :'a list ) : 'a t_avl =
  match l with
    [] -> bt_empty()
  |
    e::r -> avl_ajt(avl_lbuild(r),e) 
                      
;;




