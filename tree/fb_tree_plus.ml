(** NOMBRE: HMIA									
												  *)


(*#load "fb_tree.cmo";;*)
open Fb_tree;;

(*val is_single : 'a Fb_tree.fb_tree -> bool (* identifica árboles-hoja *)*)
let is_single tree =
	try let _ = branches tree in false 
	with Branches->true;;

	 
(*val l_branch : 'a Fb_tree.fb_tree -> 'a Fb_tree.fb_tree (* rama izquierda *)*)	 
let l_branch tree = let (hijoI, _) = branches tree in hijoI;;


(*val r_branch : 'a Fb_tree.fb_tree -> 'a Fb_tree.fb_tree (* rama derecha *)*)
let r_branch tree = let (_, hijoD) = branches tree in hijoD;;

(*val size : 'a Fb_tree.fb_tree -> int (* número de nodos *)*)
let rec size tree = match (is_single tree) with
  true -> 1
  |_ -> 1 + (size (l_branch tree) + size (r_branch tree));;
 
(*val height : 'a Fb_tree.fb_tree -> int (* altura *)*) 
let rec height tree = match (is_single tree) with
	true ->1
	|_-> 1 + (max (height(l_branch tree)) (height (r_branch tree)));;

(*val preorder : 'a Fb_tree.fb_tree -> 'a list= <fun>, (*Nodo raíz, nodo izquierda, nodo derecha*)*)

let preorder tree=
	 let rec aux  l tree = match (is_single tree) with
	  true-> (root tree)::l
	  |false-> let newL = aux ((root tree)::l) (l_branch tree) in 
		       aux  newL (r_branch tree)
	 in List.rev (aux [] tree) ;;
	 

(*val postorder : 'a Fb_tree.fb_tree -> 'a list, (Nodo izquierda, nodo derecha, nodo raíz)*)
let rec postorder tree = match (is_single tree) with
    true-> [root tree]
	|false->postorder(l_branch tree) @ postorder(r_branch tree) @ [root(tree)];;
(*val postorder : 'a fb_tree -> 'a list = <fun>*)

(*val inorder : 'a Fb_tree.fb_tree -> 'a list (Nodo izquierda, nodo raíz y nodo derecha)*)         
let rec inorder tree = match (is_single tree) with
    true-> [root tree]
	|false->(inorder (l_branch tree))@[root tree]@(inorder (r_branch tree));; 
(* val inorder : 'a fb_tree -> 'a list = <fun>*)	

(*val leafs : 'a Fb_tree.fb_tree -> 'a list (* lista de hojas *)*)	 
let rec leafs tree = match (is_single tree) with
   	   true->[root tree]
   	   |false->leafs (l_branch tree) @ leafs (r_branch tree);;    	   
(*val leafs : 'a fb_tree -> 'a list = <fun>*)

(*val mirror : 'a Fb_tree.fb_tree -> 'a Fb_tree.fb_tree (* imagen especular *)*)
let rec mirror tree = match (is_single tree) with
	true->single (root tree)
	|false->let rd=mirror(r_branch tree) in
			let ri=mirror(l_branch tree) in
		    comp (root tree) (rd, ri);;
(*val mirror : 'a fb_tree -> 'a fb_tree = <fun>*)

(*val treemap : ('a -> 'b) -> 'a Fb_tree.fb_tree -> 'b Fb_tree.fb_tree
(* aplica una función a todos los nodos *)*)
let rec treemap f tree = match (is_single tree) with 
    true->single (f(root tree))    
   |false-> comp (f(root tree)) (treemap f (l_branch tree) , treemap f (r_branch tree ) );;
(*val treemap : ('a -> 'b) -> 'a fb_tree -> 'b fb_tree = <fun>*)

(*val is_perfect : 'a Fb_tree.fb_tree -> bool (* un árbol lleno es perfecto si 
todas sus hojas están en el mismo nivel *)*)
let rec is_perfect tree= match (is_single tree) with
   true->true
   |false-> match (height (l_branch tree) = height (r_branch tree)) with
           |true->true&&(is_perfect (l_branch tree))&&(is_perfect (r_branch tree)) 
           |false->false;;
       
(** Ejercicio nº 5 (opcional)
Utilizando el módulo Fb_tree, pero asumiendo que sólo se conoce la interfaz del módulo, y no su 
implementación interna, añada al fichero fb_tree_plus.ml la siguiente función: 
	val is_complet : 'a Fb_tree.fb_tree -> bool (* un árbol es completo si todo 
	nivel, excepto quizás el último, está lleno y todos los nodos del último 
	nivel están lo más a la izquierda posible *)
**)

let rec is_complet tree= match (is_single tree) with 
   true-> true
   |false-> match (height (l_branch tree) - height (r_branch tree)) with
        0->true&& is_complet (l_branch tree) && is_complet (r_branch tree)
       |1->true&& is_complet (l_branch tree) && is_complet (r_branch tree)
       |_->false;;
       
       
       
           


 
        
        
        
        
        
