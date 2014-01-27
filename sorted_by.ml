(**
  NOMBRE:  HMIA
**)	  										 **)
open List;;
(*Defina una función sorted : 'a list -> bool que indique, para cada lista, si sus elementos están 
ordenados según la relación de orden habitual (<=).*)
let sorted l =
	let rec sor head l=match l with
	    |[]->true
	    |h::t->if head<=h then true&&(sor h t) else false
	in 
		sor (hd l) l;;
(**val sorted : 'a list -> bool = <fun>**)

(*Una relación de orden en un tipo 'a puede representarse mediante una función de tipo 'a -> 'a -> bool.
Defina una función sorted_by : ('a -> 'a -> bool) -> 'a list -> bool, que sirva para comprobar si una lista 
está ordenada mediante una relación de orden dada. En general, sorted_by r [a1; a2; a3;...;an-1;an]
debería ser equivalente a r a1 a2 && r a2 a3 && ... && r an-1 an (es decir, cada elemento de la lista debe 
estar relacionado, mediante r, con el siguiente elemento) .*)

let rec sorted_by f l=
	let rec functionaux head l=match l with
	    |[]->true
	    |h::t->if (f head h)=true then true&&(functionaux h t) else false
	in 
		functionaux (hd l) (tl l);;
(**val sorted_by : ('a -> 'a -> bool) -> 'a list -> bool = <fun>**)

(*Redefina la función sorted, del primer apartado, utilizando la función sorted_by.*)
let sorted l=sorted_by (<=) l;;
(**val sorted : 'a list -> bool = <fun>**)

(*Defina las siguientes funciones utilizando sorted_by:*)
	(**crec : 'a list -> bool (* indica si la lista está en orden creciente *)**)
	let crec l=sorted_by (<=) l;;
	(**st_crec : 'a list -> bool (* indica si la lista es estrictamente creciente **)*)
	let st_crec l=sorted_by (<) l;;
	(**decr : 'a list -> bool (* indica si la lista está en orden decreciente *)**)
	let decr l=sorted_by (>=) l;;
	(**st_decr : 'a list -> bool (* indica si la lista es estrictamente decreciente *)**)
	let st_decr l=sorted_by (>) l;;
	(**plana : 'a list -> bool (* indica si todos los elementos de la lista son iguales *)**)
	let plana l=sorted_by (=) l;;
	(**doblada: int list -> bool (* indica si cada elemento de la lista es el doble del anterior *)**)
	let doble a elemento=
		if elemento=(a*2) then true
		else false;;
	let doblada l=sorted_by (doble) l;;
	(**val doblada : int list -> bool = <fun>**)

(*Dado s : string, en ocaml podemos referirnos al carácter que ocupa la posición i-ésima del string
utilizando la notación s.[i] (donde i debe ser una expresión de tipo int tal que 0 <= i < String.length s).*)

(**char_in_st : char -> string -> bool (* indica si un carácter aparece en un string *)**)

	let char_in_st c s=
	  let l=String.length s in
	  let rec loop i=match (i<l) with
	     false->false
	    |true-> 	if (s.[i]=c) then true else loop (i+1)
	  in loop 0;;	
	(**val char_in_st : char -> string -> bool = <fun>**)
	
		

(*st_search_fst : string -> char -> int (* devuelve el índice de la primera aparición de un carácter en un 
string; st_search_fst s c activa la excepción Failure "st_search_fst" si char_in_st c s es false *)*)

	let st_search_fst s c=
		if(char_in_st c s)=true then 
			(       let rec loop i=
				if s.[i]=c then i else loop (i+1)
				in loop 0
		         )
		else raise (Failure "st_search_fst");;
	(**val st_search_fst : string -> char -> int = <fun>**)


(*st_search_all : string -> char -> int list (* devuelve la lista de todas las posiciones en las que aparece 
un carácter dado dentro de un string, en orden creciente *)*)
	let st_search_all s c=
	   	let rec search s c l i =match i with
		    |(-1)->l
                    |_->if s.[i]=c then search s c (i::l) (i-1)
			else
				search s c l (i-1)
 		in search s c [] ((String.length s)-1);;	
(**val st_search_all : string -> char -> int list = <fun>**)


































		
