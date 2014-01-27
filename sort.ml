(**
  NOMBRE:  HMIA
**)	  
open List;;

(** ¿Es recursiva terminal? Si no lo fuera, ¿qué inconveniente podría tener?
Si no lo es, haga una versión recursiva terminal de la "misma función". **)
(*r= intervalo de numeros aleatorios, l=numero de numeros aleatorios, longitud*)
let rec rlist r l =
 if l <= 0 then []
 else Random.int r :: rlist r (l-1);;
 
 (*No no es terminal, sigue dejando operaciones por hacer Random.int r::rlist r (l-1) *)
 
 let rlistT r l=
    let rec rlist r l lista=match (l<=0) with
    	 true->lista
        |false->rlist r (l-1) (Random.int r::lista)
    in rlist 
   			r l [];;
 
 (*El inconveniente es que el Randon.int va acumulando operaciones en la pila *)
(**¿Son ambas implementaciones recursivas terminales? Si no lo son, ¿supone eso en la práctica
algún tipo de limitación?**)
   			
let rec remove x = function 
	 [] -> []
	 | h::t -> if x = h then t else h::remove x t;;
	 
	 
let lmin (h::t) = fold_left min h t;; (*EL minimo de una lista llena, para el caso de la lista vacia no existe minimo*)

let rec s_sort = function
	 [] -> []
	 | l -> let m = lmin l in
	 let resto = remove m l in
 m::s_sort resto;;
 
let rec insert x = function 
	 [] -> [x]
	 | h::t -> if x <= h then x::h::t
 else h::insert x t;;
 
let rec i_sort = function 
	 [] -> []
	 | h::t -> insert h (i_sort t);;   		
	 
(*No no son recursiva terminal, *)


(*Compare empíricamente la rapidez en la aplicación de ambos métodos. Puede utilizar para ello
la "pseudo-función" rlist definida anteriormente y la "pseudo-función" crono definida a continuación*)
let crono f x =
	 let t = Sys.time () in
	 let _ = f x 
in Sys.time () -. t;;

map (crono s_sort) 
 	(map (rlistT 100000) [1000;2000;4000;8000;16000]);;
(* - : float list =
[0.0720039999999999847; 0.320021; 1.320083; 5.372335; 21.44534]*)

map (crono i_sort) 
 	(map (rlistT 100000) [1000;2000;4000;8000;16000]);;
(*- : float list =
[0.0200010000000006; 0.0680039999999984; 0.280017000000000849;  1.17207399999999851; 5.524345]*)

(** ¿Cuál es más rápida? La funcion i_sort es mucho más rápida que s_sort*)
(** ¿Cómo crece el tiempo de ejecución con la longitud de la listas? Casi 4 veces mas le lleva s_sort ordenar que i_sort **) 
(**¿Existen casos especialmente "fáciles" o "difíciles"?**)

let aleatoria=rlistT 2000 10000;;
let creciente=i_sort aleatoria;;
let descreciente=rev(creciente);;

crono i_sort aleatoria;;
(*- : float = - : float = 2.06012900000000343*) 
(**Hice varias ejecuciones sobre i_sort con la lista aleatoria y casi todas me dan entorno de 2.*)
crono i_sort creciente;;
(*- : float = 0.00400099999999525835*)
crono i_sort descreciente;;
(*- : float = 3.90424300000000102*)

crono s_sort aleatoria;;
(**Hice varias ejecuciones sobre s_sort con la lista aleatoria y casi todas me dan entorno de 8.*)
(*- : float = 8.04050300000000107 *)
crono s_sort creciente;;
(*- : float = 3.60422499999999957*)
crono s_sort descreciente;;
(*- : float = 13.7968620000000044 *)
(**¿Fáciles?   para i_sort es cuando la lista está ordenada en orden creciente**)
(**¿Díficiles? En el caso de s_sort es cuando la lista está en orden descreciente, y i_sort también porque se duplica su tiempo en comparación con la i_sort de la lista aleatoria*)

(**Si determinó que las implementaciones anteriores no eran recursivas terminales, realice 
implementaciones alternativas (t_s_sort y t_i_sort) que sí lo sean. ¿Presentan en la práctica 
alguna ventaja? ¿Y algún inconveniente? ¿Cuáles son más rápidas?**)

(*Si la implementación terminal realizada para el método de inserción resultase más lenta que la 
original, intente mejorarla de modo que no lo sea. Debe tratarse, en todo caso, de una 
implementación recursiva terminal basada en el método de inserción.*)































	 
 
 
 
