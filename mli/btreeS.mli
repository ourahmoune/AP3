(* le type abstrait *)
type 'a btree ;;
(*bt_isempty*)
val bt_isempty : 'a btree -> bool  ;; 
(*bt_empty*)
val bt_empty  : unit -> 'a btree ;; 
val bt_rooting  : 'a * 'a btree * 'a btree ->  'a btree ;; 
val bt_root  : 'a btree -> 'a  ;; 
val bt_subleft  : 'a btree -> 'a btree ;; 
val bt_subright  : 'a btree -> 'a btree ;; 
val hateur : 'a btree -> int ;;
val desiquilibre : 'a btree -> int ;;
val desiquilibre_total: 'a btree -> int ;;
val max : 'a * 'a -> 'a ;;
val desiquilibre_moyen : 'a btree * int  -> float ;;

