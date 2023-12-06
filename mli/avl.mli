open BtreeS ;; 
open Bst ;; 
type 'a t_avl  ;;
val ab_rg : 'a t_avl -> 'a t_avl ;;
val ab_rd : 'a t_avl -> 'a t_avl ;;
val ab_rgd : 'a t_avl -> 'a t_avl ;;
val ab_rdg : 'a t_avl -> 'a t_avl ;;
val ab_reequilibre : 'a t_avl -> 'a t_avl ;;
val avl_ajt :'a t_avl * 'a -> 'a t_avl ;; 
val avl_seek :'a t_avl * 'a -> bool ;; 
val avl_max :'a t_avl -> 'a*int ;;
val avl_sup_max : 'a t_avl -> 'a t_avl ;; 
val avl_sup : 'a t_avl * 'a -> 'a t_avl ;; 
val avl_lbuild : 'a list -> 'a t_avl ;;
 
