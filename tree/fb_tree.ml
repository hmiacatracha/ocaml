(**																				
   HMIA																			
   GRUPO																		
   HORARIO: 																	
   PRACTICA 																	
								 											 **)

exception Branches;;

(*'a fb_tree que sirva para representar “árboles binarios llenos”
 (full binary trees) con nodos etiquetados con valores de tipo 'a.”*)

type 'a fb_tree=
	Leaf of 'a
	|Node of 'a * 'a fb_tree* 'a fb_tree;;
	
(*let arbol=Node(1,Node(1,Leaf 2, Leaf 3),Node(1,Leaf 4, Leaf 5));;*)
(*let arbol2=comp 1 (comp 1 (single 2) (single 3)) (comp 1 (single 4) (single 5));;*)


(*La función string_of_tree servirá para “visualizar como string” el contenido de un árbol. Habrá que 
pasarle como parámetro una función que permita convertir a strings las etiquetas de los nodos. Esta 
función se incluirá en el módulo sólo a efectos de facilitar la “visualización” de árboles para la realización 
de pruebas y no debe utilizarse en los desarrollos que se pedirán más adelante*)
(*string_of_tree string_of_int arbol;;*)

let rec string_of_tree f = function
 Leaf a ->
 "(" ^ (f a) ^ ")"
 | Node (a, t1, t2) ->
 "(" ^ (f a) ^ " " ^ (string_of_tree f t1)
 ^ " " ^ (string_of_tree f t2) ^ ")";;	
(*val string_of_tree : ('a -> string) -> 'a fb_tree -> string*)

	
(*La función single servirá para construir un árbol-hoja con una etiqueta dada.*)
let single a= Leaf a;;
(*val single : 'a -> 'a fb_tree = <fun>*)


(*La función comp servirá para construir un árbol a partir de una etiqueta para la raíz y de un par de árboles
que serán sus ramas*) (*comp 1, arbol, arbol*)
let comp etiqueta (arb1, arb2)=
  Node(etiqueta, arb1, arb2);;
(*val comp : 'a -> 'a fb_tree -> 'a fb_tree -> 'a fb_tree = <fun>*)


(*La función root devolverá el valor (etiqueta) de la raíz de un árbol.*)
let root tree=match tree with
	Leaf r->r
	|Node(r,_,_)->r;;
(*val root : 'a fb_tree -> 'a = <fun>*)

(*La función branches devolverá las ramas de un árbol. Si se aplica a un árbol que no tiene ramas (un 
árbol-hoja) se activará la excepción Branches.*)

let branches tree=match tree with
  Leaf a->raise Branches
  |Node(_,ri,rd)->(ri,rd);;
(*val braches : 'a fb_tree -> 'a fb_tree * 'a fb_tree = <fun>*)


    
    
    
    
    

