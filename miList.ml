(**
  NOMBRE:  HMIA
**)	  

(**Redefina las funciones hd, tl, length, rev, nth y append, del módulo List, sin utilizar ese módulo ni
la función ( @ ) del módulo Pervasives **)

(*Devuelve el primer elemento de cualquier tipo de lista*)
let hd l= match l with
  h::_->h
  |[]->raise (Failure "hd");;
(*-:'a list->'a= <fun>*)

(*Devuelve todos los elementos de una lista menos la cabeza*)
let tl = function
	[] -> raise (Failure "tl")
	|h::t -> t;;
(*- : 'a list -> 'a list = <fun>*)

(*Devuelve el tamaño de la lista*)
let length l=
   let rec aux l acum=match l with
   []->acum
   |h::t->aux t (acum+1)
  in aux l 0;;
(*val length : 'a list -> int = <fun>*)

(*Devuelva el n-ésimo elemento de la lista dada. El primer elemento está en la posición 0*)
let rec nth l n =
	match l with
        []->raise (Failure "nth")
		|h::t-> match n with 	
			0->h
			|_->nth t (n-1);;
(*val nth : 'a list -> int -> 'a = <fun>*)	
			
(* Concatenar dos listas. La misma función que el operador infijo @. 
 No recursiva de cola (longitud del primer argumento)*)
let append l1 l2=match l2 with
	[]->l1
	|h::t->let l=List.rev l1 in
	       let rec aux l l2=match l with
	        []->l2
	        |h::t->aux t (h::l2)
	    in
		aux l l2;;
(*val append : 'a list -> 'a list -> 'a list = <fun>*)

(**Redefina las funciones rev, rev_append, concat, flatten, map, map2, fold_left y fold_right, del 
módulo List, sin utilizar ese módulo.**)
(*Le pasa como parámetro una lista y te devuelve otra lista en orden inverso de la del parámetro*)
let rev l =
	let rec revO l a = match l with
	| [] -> a
	| h :: t -> revO t (h :: a)
in revO l [];;
(*val rev : 'a list -> 'a list = <fun>*)

(*rev_append l1 l2, l1 invierte y concatena a l2. 'a list -> 'a list -> 'a list*)
let rec rev_append l1 l2=match l1 with
	[]->l2
	|h::t->rev_append t (h::l2);;
(*- : 'a list -> 'a list -> 'a list = <fun>*)
	
(*Concatenar una lista de listas. Los elementos del argumento son todos concatenan juntos
(en el mismo orden) para dar el resultado)*)
let rec concat l1=match l1 with
    []->[]
    |h::t->append h (concat t);;
(*val concat : 'a list list -> 'a list = <fun>*)
 
let flatten l=concat l;;
(*val flatten : 'a list list -> 'a list = <fun>*)

(*List.map f [a1, ..., an] aplica la función f para a1, ..., an, 
y construye la lista [f a1, ..., f an] 
con los resultados devueltos por f. Not tail recursive.*)
let rec map f l=match l with
	[]->[]
	|h::t->(f h)::(map f t );;
(*val map : ('a -> 'b) -> 'a list -> 'b list = <fun>*)

(*List.map2 f [a1, ..., an] [b1, ..., bn] es [f b1 a1, ..., f un bn]. 
 invalid_argument si las dos listas tienen longitudes diferentes. Not tail recursive*)
let rec map2 f l1 l2=match (l1,l2) with
	([],[])->[]
	|([],_::_)-> raise (Failure "Invalid_argument")
	|(_::_,[])-> raise (Failure "Invalid_argument")
	|(h1::t1, h2::t2)->f h1 h2::(map2 f t1 t2);;
(*val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list = <fun>*)

(*fold_left f a [b1; ...; bn] es f (... (f (f a b1) b2) ...) bn.*)
let rec fold_left f x =function 
	|[]->x
	|h::t-> fold_left f (f x h) t;;  
(*val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>*)    
	
(* fold_right f [a1; ...; an] b es f a1 (f a2 (... (f an b) ...)). Not tail-recursive*)	
let rec fold_right f l x = match l with
  | [] -> x
  |h::t -> f h (fold_right f t x);;
(*val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b = <fun>*)


(**Redefina las funciones find, for_all, exists, mem, filter, find_all, partition, split y combine del 
módulo List, sin utilizar ese módulo.**)

(*encontrar j devuelve el primer elemento de la lista L que satisfaga el predicado p.*)
let rec find p l =match l with 
	|[]->raise (Failure "Not_found")
	|h::t->if p(h) then h else find p t;; 
	(* ('a -> bool) -> 'a list -> 'a = <fun>*)

(*comprueba si todos los elementos de la lista satisfacen el predicado p. 
Es decir, se devuelve (p a1) && (p a2) && ... P && (a).*)
let rec for_all p l=match l with
	[]->true
	|h::t->if p(h) then true&&(for_all p t) else false;;
(*val for_all :s ('a -> bool) -> 'a list -> bool = <fun>*)


(*comprueba si al menos un elemento de la lista satisface el predicado p. 
Es decir, se devuelve (p a1) | | (p a2) | | ... | | (P s)*)

let rec exists p l=match l with
	|[]->false
	|h::t->if p(h)=true then true else false||exists p t;;
(*val exists : ('a -> bool) -> 'a list -> bool = <fun>*)

(* mem a l es verdadero si y sólo si a es igual a un elemento de l.*)
let rec mem a l=match l with
	[]->false
	|h::t->if h=a then true else (mem a t);;
(*val mem : 'a -> 'a list -> bool = <fun>*)

(*devuelve todos los elementos de la lista l que satisfacen el predicado p.
El orden de los elementos en la lista de entrada se conserva. *)
let rec filter p l=match l with
		[]->[]
		|h::t->if p(h)=true then h::(filter p t) else filter p t;;
(*val filter : ('a -> bool) -> 'a list -> 'a list = <fun>*)

(*Hace lo mismo que filter*)
let find_all = filter;;  
(*val find_all : ('a -> bool) -> 'a list -> 'a list = <fun>*)
 
(*devuelve un par de listas (L1, L2), donde l1 es la lista de todos los 
elementos de l que satisfacen el predicado p, y L2 es la lista de todos 
los elementos de l que no satisfagan p.
El orden de los elementos en la lista de entrada se conserva*) 
let partition p l=let rec partition1 p l l1 l2=match l with
		|[]->l1,l2
		|h::t->if p(h)=true then partition1 p t (h::l1) l2 else partition1 p t l1 (h::l2)
	in partition1 p l [] [];;   
(*val partition : ('a -> bool) -> 'a list -> 'a list * 'a list = <fun>*)

(*split [(a1, b1), ..., (an, bn)] es ([a1, ..., an], [b1, ..., bn] )
Not tail-recursive.*)
let split l=let rec split1 l l1 l2=match l with
	    |[]->rev l1, rev l2
	    |(x,y)::t-> split1 t (x::l1) (y::l2)
	in split1 l [] [];;
(*val split : ('a * 'b) list -> 'a list * 'b list = <fun>*)

(*Otra version*)
let rec split l = match l with
	[] -> ([],[])
	|(x,y)::t -> let l1,l2 = split t in
					(x::l1,y::l2);; 
(*val split : ('a * 'b) list -> 'a list * 'b list = <fun>*)

(*combine [a1; ...; an] [b1; ...; bn] es [(a1,b1); ...; (an,bn)]. Invalid_argument si las listas tienen diferentes longitud*)
let rec combine a b=
	match a,b with
		[],[]->[]
		|h1::t1,[]->raise (Failure "Invalid_argument")
		|[],h2::t2->raise (Failure "Invalid_argument")
		|h1::t1,h2::t2->let l=combine t1 t2 in
				(h1,h2)::l;;
(*val combine : 'a list -> 'b list -> ('a * 'b) list = <fun>*)

(**Defina una función remove: 'a -> 'a list -> 'a list, que “elimine la primera aparición, si la hay, de 
un valor en una lista”; de forma que, por ejemplo remove 3 [2; 6; 3; 4; 3] sea la lista [2; 6; 4; 3] y 
remove 3 [1; 2; 4] sea la lista [1; 2; 4] **)

let rec remove a l=
	match l with
	[]->[]
	|h::t->if h=a then t else h::remove a t;;

(*val remove : 'a -> 'a list -> 'a list = <fun>*)

(**Defina una función remove_all: 'a -> 'a list -> 'a list, que “elimine todas las apariciones de un valor 
en una lista”; de forma que remove_all 3 [2; 6; 3; 4; 3] sea la lista [2; 6; 4].**)

let rec remove_all a l =match l with
	[]->[]
	|h::t->if h=a then remove_all a t else h::remove_all a t;; 

(*val remove_all : 'a -> 'a list -> 'a list = <fun*)
	
(**Defina una función ldif: 'a list -> 'a list -> 'a list, de forma que ldif l1 l2 elimine de l1 todas las 
apariciones de todos aquellos valores que aparezcan en l2. Así, por ejemplo, ldif [1;2;3;2;4] 
[2;3;3;5] debería ser la lista [1;4]**)

let rec ldif l1 l2=match l2 with
	[]->l1
	|h2::t2->ldif (remove_all h2 l1) t2;;	    
(*val ldif : 'a list -> 'a list -> 'a list = <fun>*)

(**Defina una función lprod: 'a list -> 'b list -> ('a * 'b) list, de forma que lprod l1 l2 calcule el 
“producto cartesiano” de l1 y l2. Así, por ejemplo, lprod [1;3;1;2] ['a';'b'] debería ser la lista 
[(1,'a'); (1,'b'); (3,'a'); (3,'b'); (1,'a'); (1,'b'); (2,'a'); (2,'b')].**)
	let rec producto h l=match l with
		[]->[]
		|h1::t1->(h,h1)::(producto h t1);;
	 
	let rec lprod l1 l2=match l1 with
		[]->[]
		|h1::t1->append (producto h1 l2) (lprod t1 l2);;
(*val lprod : 'a list -> 'b list -> ('a * 'b) list = <fun>*)




