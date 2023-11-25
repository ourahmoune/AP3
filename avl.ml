#load "btreeS.cmo";;
open BtreeS ;;

let ab_rg (a: 'a btree ): 'a btree = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rg non definie sur les arbre vide "
    else
        let fd = bt_subright(a) in 
        if(bt_isempty(fd)) then 
            failwith"error : ab_rg est non definie sur les arbre qui n'ont pas de fils droite"
        else    
            let p = bt_root(a) and 
                u = bt_subleft(a) and 
                q= bt_root(fd) and 
                v= bt_subleft(fd) and 
                w= bt_subright(fd) in 
            bt_rooting(q,bt_rooting(p,u,v),w)
;;

let ab_rd (a: 'a btree ): 'a btree = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rg non definie sur les arbre vide "
    else
        let fg = bt_subleft(a) in 
        if(bt_isempty(fg)) then 
            failwith"error : ab_rd est non definie sur les arbre qui n'ont pas de fils gauche"
        else    
            let q = bt_root(a) and 
                w = bt_subright(a) and 
                p= bt_root(fg) and 
                u= bt_subleft(fg) and 
                v= bt_subright(fg) in 
            bt_rooting(p,u,bt_rooting(q,v,w))
;;

let ab_rgd (a: 'a btree ): 'a btree = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rgd non definie sur les arbre vide "
    else
        let fg = bt_subleft(a) in 
        if(bt_isempty(fg)) then 
            failwith"error :ab_rgd  est non definié sur les arbre qui n'ont pas de fils gauche "
        else
            let fdg = bt_subright(fg) in 
            if( bt_isempty(fdg)) then 
                failwith"error : ab_rgd est non definié sur les arbre qui n'ont pas un fils droite de fils gauche"
            else
                let r = bt_root(a) and 
                    p = bt_root(fg) and 
                    t= bt_subleft(fg) and
                    q= bt_root(fdg) and 
                    u=bt_subleft(fdg) and 
                    v=bt_subright(fdg)and 
                    w=bt_subright(a) in 
                bt_rooting(q,bt_rooting(p,t,u) ,bt_rooting(r,v,w))

;;

let ab_rdg (a: 'a btree ): 'a btree = 
    if(bt_isempty(a)) then 
        failwith"error : ab_rdg non definie sur les arbre vide "
    else
        let fd = bt_subright(a) in 
        if(bt_isempty(fd)) then 
            failwith"error :ab_rdg  est non definié sur les arbre qui n'ont pas de fils droite "
        else
            let fgd = bt_subleft(fd) in 
            if( bt_isempty(fgd)) then 
                failwith"error : ab_rdg est non definié sur les arbre qui n'ont pas un fils gauche de fils droite"
            else
                let r = bt_root(a) and 
                    p = bt_root(fd) and 
                    t= bt_subleft(a) and
                    q= bt_root(fgd) and 
                    u=bt_subleft(fgd) and 
                    v=bt_subright(fgd)and 
                    w=bt_subright(fd) in 
                bt_rooting(q,bt_rooting(r,t,u) ,bt_rooting(p,v,w))

;;
type 'a t_avl = ('a * int ) btree ;;
let  ab_reequilibre (a: 'a t_avl) : 'a t_avl =
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
              failwith "error -----------"
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

let rec avl_ajt (a , e : 'a t_avl * 'a ) : 'a t_avl =
  if(bt_isempty(a)) then
    bt_rooting((e,0),bt_empty(),bt_empty())
  else
    let (r,d) = bt_root(a) in
    if (e<r ) then
      ab_reequilibre(bt_rooting((r,d+1) ,avl_ajt(bt_subleft(a),e),bt_subright(a) ) )
    else
      if(e>r) then
        ab_reequilibre(  bt_rooting((r,d-1),bt_subleft(a) , avl_ajt(bt_subright(a),e)  ) )
      else
        a
;;
let a = avl_ajt(bt_empty(),12) ;;
let b = avl_ajt(a,3);;
let c = avl_ajt(b,2) ;;
