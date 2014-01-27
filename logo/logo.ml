(**																				
   HMIA																			
   GRUPO																		
   HORARIO: 																	
   PRACTICA 																	
								 											 **)

(*#load "graphics.cma";;*)
(*#load "logo.cmo";;*)
open Graphics;;
let ancho=600.;;
let anchoT=15.;;
let altoT=20.;;
let alto=450.;;
let pi= 3.14159265359;;
type estado={mutable x:float; mutable y:float; mutable angulo:float; mutable pd:bool; mutable vi:bool; 
             mutable img:(Graphics.image * int * int); mutable variables:(string*float)list};;
   
								 
(* Definir un tipo de dato exp ari que represente las expresiones aritmeticas
 del lenguaje.*)
	 type exp_ari= 
	 Const of float
	 |Sum  of (exp_ari*exp_ari)
	 |Res  of (exp_ari*exp_ari)
	 |Mul  of (exp_ari*exp_ari)
	 |Div  of (exp_ari*exp_ari)
	 |Mod  of (exp_ari*exp_ari)
	 |Exp  of (exp_ari*exp_ari)
	 |Op   of (exp_ari)
	 |Var  of string;;
	
		
(*Hay que definir una función eval_exp_bool: estado -> exp_bool 
-> boolean*) 
	
	type exp_bool =
     V                           
   | F                                
   | Not of exp_bool               
   | And of exp_bool * exp_bool 
   | Or of exp_bool * exp_bool  
   | Igual of exp_ari * exp_ari
   | Menor of exp_ari * exp_ari         
   | Mayor of exp_ari * exp_ari 
   | MayorI of exp_ari * exp_ari       
   | MenorI of exp_ari * exp_ari;;      

	
 
(*Implementar un evaluador de expresiones aritmeticas, es decir, una funcion
eval exp: exp ari -> float que dado un valor de tipo exp ari calcule
el resultado de la expresion.*)
    let f nombreBuscado (nombre,valor) = 0 == String.compare nombreBuscado nombre;;

	let rec eval_exp estado exp = match exp with 
	   Const a -> a
	   |Sum(a,b) -> (eval_exp estado a) +. (eval_exp estado b) 
	   |Res(a,b) -> (eval_exp estado a) -. (eval_exp estado b) 
	   |Mul(a,b) -> (eval_exp estado a) *. (eval_exp estado b) 
	   |Div(a,b) -> (eval_exp estado a) /. (eval_exp estado b) 
	   |Mod(a,b) -> mod_float (eval_exp estado a) (eval_exp estado b) 
	   |Exp(a,b) -> (eval_exp estado a) ** (eval_exp estado b) 
	   |Op (a)   -> (eval_exp estado a) *. (-1.)
	   |Var a-> snd(List.find (f a) estado.variables);;
	   
	(*val eval_exp : estado -> exp_ari -> float = <fun>*)   

    let rec eval_exp_bool estado exp=match exp with
     V        -> true
    |F        -> false
    |Not      a-> not (eval_exp_bool estado a)
    |And     (a,b)->(eval_exp_bool estado a) && (eval_exp_bool estado b)
    |Or      (a,b)->(eval_exp_bool estado a) or (eval_exp_bool estado b)
    | Igual  (a,b)->(eval_exp estado a)=(eval_exp estado b)
    | Menor  (a,b)->(eval_exp estado a)<(eval_exp estado b)         
    | Mayor  (a,b)->(eval_exp estado a)>(eval_exp estado b)
    | MayorI (a,b)->(eval_exp estado a)>=(eval_exp estado b)      
    | MenorI (a,b)->(eval_exp estado a)<=(eval_exp estado b);;
    
    (*val eval_exp_bool : estado -> exp_bool -> bool = <fun>*)
	
  
(*Definir un tipo de dato estado que represente el estado del programa en
	un momento determinado. Definir tambien un estado inicial que tenga a
	la tortuga en el centro de la pantalla, mirando hacia arriba y con el lapiz
	bajado
 *)
   
(*Definir un tipo de dato programa que represente las instrucciones del
	lenguaje*)
type programa=
  |Borra 
  |Avan of exp_ari  
  |Retr of exp_ari  
  |GIzq of exp_ari
  |GDer of exp_ari
  |Sub 
  |Baj  
  |EjecInts of programa list
  |Repeat   of exp_ari*programa
  |Print    of exp_ari
  |Make     of string * exp_ari  
  |While    of exp_bool * programa
  |ColorLapiz of exp_ari             
  |TamLapiz   of exp_ari
  |MostrarTort                         
  |OcultarTort;;
                           
         
(*Implementar un ejecutador de expresiones del lenguaje 
	(ejecutar: estado -> programa -> estado), 
que dado el estado actual del programa y una
instruccion ejecute esa instruccion cambiando el estado 
y dibujando en una ventana grafica si es necesario.*)

(*Funcion que calcule el proximo X e Y*)
let radianes grados=
     (2. *. pi *. grados)/.360.;;
     
let grados radianes= 
     90. -. ((radianes *. 360.) /. (2. *. pi));; (* la tortuga esta rotada 90 grados *)
     
let getX angulo n = n *. cos(angulo);;
let getY angulo n = n *. sin(angulo);;

let truncate n=
       let decimal = truncate n in
       match ((n -. float_of_int(decimal))>=0.5) with
       |true->(decimal+1)
       |false ->decimal;;
       

(*Hay que evaluar la n antes de usarla como expresion aritmetica*)
(**                           EJECUTAR                                          
                                                         						
                                                         						
                                                                              *)
                                                                              
let rec ejecutar estado programa = match programa with
 |Borra  ->clear_graph();
          moveto 300 225;
          estado.x<-(ancho /. 2.);
          estado.y<-(alto /. 2.);
          estado.angulo<-pi/. 2.;
          estado.pd<-true; 
          estado.vi<-true;
          estado.variables<-[];
          estado.img <- Turtle.drawturtle (estado.x, estado.y) 0. anchoT altoT  estado.vi estado.pd; 
          estado
 |Avan n -> 
            Turtle.redraw estado.img;
            let n_eval= eval_exp estado n in
            let x_destino = (getX estado.angulo n_eval) +. estado.x  in
            let y_destino = (getY estado.angulo n_eval) +. estado.y  in            
		        (
		        match estado.pd with
		         true -> lineto (truncate x_destino) (truncate y_destino)
				 |false -> moveto (truncate x_destino) (truncate y_destino);
				);				
		    estado.x<-x_destino;
		    estado.y<-y_destino;            
            estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
       	    estado
 |Retr n ->
           Turtle.redraw estado.img;
           let n_eval= eval_exp estado n in
           let x_destino = estado.x -. (getX estado.angulo n_eval) in 
           let y_destino = estado.y -. (getY estado.angulo n_eval) in                   
             (
              match estado.pd with
                    true ->  lineto (truncate x_destino) (truncate y_destino)
		    		|false -> moveto (truncate x_destino) (truncate y_destino);
		     );
		   estado.x<-x_destino;
		   estado.y<-y_destino;                   
           estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
       	   estado
		       		  
 |GDer n -> 
	 		Turtle.redraw estado.img;
	 		let n_eval = eval_exp estado n in
		    estado.angulo<-(estado.angulo -. radianes( n_eval));   
            estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
		    estado 
 
 |GIzq n -> 
		       Turtle.redraw estado.img;
		       let n_eval = eval_exp estado n in
	 		   estado.angulo<-(estado.angulo +. radianes(n_eval));		       
		       estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
		       estado 
 
 |Sub  ->       estado.pd<-false;
 				Turtle.redraw estado.img;
                estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
          		estado
 
 |Baj  ->   	estado.pd<-true;
 				Turtle.redraw estado.img;
                estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
                estado
 
 |EjecInts l-> (
                 match l with 
                 h::[]-> ejecutar estado h;
                 |h::t -> let estadoNuevo = ejecutar estado h in
                     ejecutar estadoNuevo (EjecInts t);
                 |_-> estado
               )                         
 |Repeat  (n, programa)-> ( 
                let n_eval = truncate( eval_exp estado n )in
                match (n_eval>0) with
           		true->let estadoNuevo = ejecutar estado programa in
                      ejecutar estadoNuevo (Repeat (Const (float_of_int(n_eval - 1)),programa));
                |_->estado
            )
 |While (cond,programa)->
                      (
    					let condicion= eval_exp_bool estado cond in
    					match condicion with
    					true->let estadoNuevo= ejecutar estado programa in
    					      ejecutar estadoNuevo (While (cond, programa));
    					|_->estado
    				  )            
    				  
 |Print n-> 
			 Turtle.redraw estado.img;			
 			 let n_eval = (eval_exp estado n) in
             Graphics.draw_string (string_of_float n_eval);             
             estado.x <- float_of_int (Graphics.current_x());
             estado.y <- float_of_int (Graphics.current_y());
             estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
             estado
  
                   
  |Make (nombre,valor)->
                        let elemento= (nombre,(eval_exp estado valor)) in
                        let listaVariables =estado.variables in	
                        let nuevaLista =elemento::listaVariables in
                            estado.variables<-nuevaLista;
                        estado
  |MostrarTort -> 
                 estado.vi <- true;
                 Turtle.redraw estado.img;
                 estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
  	             estado
  			                       
  |OcultarTort ->
                estado.vi <- false; 
                Turtle.redraw estado.img;
                estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
  		        estado
                               
  |ColorLapiz e-> let c = int_of_float(eval_exp estado e) in
                 Graphics.set_color c;
                 Turtle.redraw estado.img;
                 estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
                 estado
  
                   
  |TamLapiz e ->let t = int_of_float(eval_exp estado e) in
  				Graphics.set_line_width t;
  				Turtle.redraw estado.img;
                 estado.img <- Turtle.drawturtle (estado.x, estado.y) (grados estado.angulo) anchoT altoT estado.vi estado.pd;
  				estado;; 
  
                         
                        
(**								PRUEBAS							                
                                                                                
																             **)
                          
                                  
      
  let p1 = EjecInts [
          Sub;
          Retr (Const 150.);
          Baj;
          GDer (Const 30.);
          Repeat ((Const 6.),(EjecInts [Avan (Const 60.); GDer (Const 60.);Avan (Const 60.); GIzq (Const 120.)]))
         ];;
                      
  let p2 =EjecInts[
				GIzq (Const 90.); 
				Repeat ((Const 6.), EjecInts [
				Repeat ((Const 3.), (EjecInts [Avan (Const 100.);GDer (Const 120.)]));
				Repeat ((Const 2.), (EjecInts [Avan (Const 100.); GDer (Const 60.); Avan (Const 100.); GDer (Const 120.)]));
				GDer (Const 60.)])
				];;
		
 let p3=  EjecInts[
		     Repeat ((Const 6.), 
		           (EjecInts [
		            GIzq(Const 60.);
		            Repeat ((Const 2.),( EjecInts [Avan (Const 60.); GDer (Const 60.); Avan (Const 60.); GIzq (Const 120.)]));
		            Avan (Const 180.); 
		            GIzq (Const 120.);
		            Repeat ((Const 2.),( EjecInts [Avan (Const 60.); GDer (Const 60.); Avan (Const 60.); GIzq (Const 120.)]));
		            ]))];;
		     
   
  
(*  Ejecutar
   				
	#load "graphics.cma";;
	#load "logo.cmo";;
	open Graphics;;
	open Logo_v0;;
	let tamVentana= " "^string_of_int(truncate(ancho))^"x"^string_of_int(truncate(alto))^"+"^"50-0";;
	Graphics.open_graph tamVentana;;
    moveto 300 225;;
ejecutar st0 p1;;
ejecutar st0 borrar;;
*)


   
   
   
   
   

   
