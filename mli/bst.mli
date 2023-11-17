open BtreeS ;;

val bst_seek : 'a btree * 'a -> bool  ;; 
val bst_insert  : 'a btree * 'a -> 'a btree ;; 
val bst_lbuild  :  'a list ->  'a btree ;; 
val bst_max  : 'a btree -> 'a  ;; 
val bst_deletemax  : 'a btree -> 'a btree ;; 
val bst_delete  : 'a btree * 'a -> 'a btree ;;