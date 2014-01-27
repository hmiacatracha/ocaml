(**																				
   HMIA																			
   GRUPO																		
   HORARIO: 																	
   PRACTICA 																	
								 											 **)

open Graphics;;
open Logo;;
open Input_program;;
  
let rec exp_ari_of exp = match exp with
|Abstract_syntax.Constante a ->Logo.Const  a 
|Abstract_syntax.Variable  v ->Logo.Var    v                 
|Abstract_syntax.Opuesto   v ->Logo.Op (exp_ari_of v)               
|Abstract_syntax.Suma      (v1, v2)->Logo.Sum  (exp_ari_of  v1, exp_ari_of  v2)
|Abstract_syntax.Resta     (v1, v2)->Logo.Res  (exp_ari_of  v1, exp_ari_of  v2 )    
|Abstract_syntax.Producto  (v1, v2)->Logo.Mul  (exp_ari_of  v1, exp_ari_of  v2)
|Abstract_syntax.Division  (v1, v2)->Logo.Div  (exp_ari_of  v1, exp_ari_of  v2) 
|Abstract_syntax.Modulo    (v1, v2)->Logo.Mod  (exp_ari_of  v1, exp_ari_of  v2)
|Abstract_syntax.Potencia  (v1, v2)->Logo.Exp  (exp_ari_of  v1, exp_ari_of  v2);;

let rec exp_bool_of exp= match exp with
|Abstract_syntax.Verdadero    ->Logo.V 
|Abstract_syntax.Falso 	      ->Logo.F 
|Abstract_syntax.Negacion    a ->Logo.Not   (exp_bool_of a)
|Abstract_syntax.Conjuncion (a,b)->Logo.And  (exp_bool_of a,exp_bool_of b)
|Abstract_syntax.Disyuncion (a,b)->Logo.Or      (exp_bool_of a, exp_bool_of b)
|Abstract_syntax.Igualdad	(a,b)->Logo.Igual   (exp_ari_of a, exp_ari_of b)
|Abstract_syntax.LT 		(a,b)->Logo.Menor   (exp_ari_of a, exp_ari_of b)
|Abstract_syntax.GT 		(a,b)->Logo.Mayor   (exp_ari_of a, exp_ari_of b)
|Abstract_syntax.LET 		(a,b)->Logo.MayorI  (exp_ari_of a, exp_ari_of b)
|Abstract_syntax.GET 		(a,b)->Logo.MenorI  (exp_ari_of a, exp_ari_of b);;


let rec program_of exp = match exp with
|Abstract_syntax.Asignar         (s, p) ->Logo.Make (s, exp_ari_of p)  
|Abstract_syntax.Secuencia         (i1,i2) ->Logo.EjecInts [program_of i1;program_of i2]
|Abstract_syntax.Mientras          (b, p)->Logo.While  (exp_bool_of b ,program_of p)
|Abstract_syntax.Repetir           (e, p)->Logo.Repeat (exp_ari_of e, program_of p)
|Abstract_syntax.Subir_lapiz           ->Logo.Sub 
|Abstract_syntax.Bajar_lapiz           ->Logo.Baj                          
|Abstract_syntax.Avanzar   		    e  ->Logo.Avan (exp_ari_of e)
|Abstract_syntax.Retroceder 		e  ->Logo.Retr (exp_ari_of e )                
|Abstract_syntax.Girar_derecha  	e  ->Logo.GDer (exp_ari_of e )             
|Abstract_syntax.Girar_izquierda    e  ->Logo.GIzq (exp_ari_of e  )
|Abstract_syntax.Borrar_pantalla       ->Logo.Borra 
|Abstract_syntax.Print              e  ->Logo.Print (exp_ari_of e)
|Abstract_syntax.Set_pen_color e ->  Logo.ColorLapiz (exp_ari_of e)             
|Abstract_syntax.Set_pen_size  e ->  Logo.TamLapiz   (exp_ari_of e)
|Abstract_syntax.Mostrar_tortuga ->  Logo.MostrarTort                         
|Abstract_syntax.Ocultar_tortuga ->  Logo.OcultarTort;;


let rec bucle_ejecucion estado = 
	try 
	let instruccionLogo = Input_program.get_inst() in
	let instruccion = program_of instruccionLogo in
	let nuevoEstado = Logo.ejecutar estado instruccion in
	bucle_ejecucion nuevoEstado (* se llama recursivamente hasta que se recibe una excepcion *)
	
	with (* Excepciones, se devuelve el estado y no se continua el bucle *)
	End_of_program ->   (); estado 
	| Error_lexico ->   print_endline("Error lexico, consulte el manual para saber mas informacion."); bucle_ejecucion estado 
	|Cannot_open_file nombre_fichero -> print_endline ("No se pudo abrir el fichero"); bucle_ejecucion estado
    |Error_sintactico-> print_endline ("Instruccion no encontrada"); bucle_ejecucion estado
    |Invalid_command_in_file->print_endline ("Instruccion del archivo no encontrada"); bucle_ejecucion estado  
;;

let ()= 
  let tamVentana= " "^string_of_int(truncate(Logo.ancho))^"x"^string_of_int(truncate(Logo.alto))^"+"^"50-0" in
  Graphics.open_graph tamVentana;
  let imagen = Turtle.drawturtle ((Logo.ancho /. 2.), (Logo.alto /. 2.)) 0. Logo.anchoT Logo.altoT true true in
  let st0 = {x=(Logo.ancho /. 2.);y=(Logo.alto /. 2.); angulo=(Logo.pi /. 2.);pd=true; vi=true;img=imagen ;variables=[]} in
  moveto 300 225;
  bucle_ejecucion st0 ;
  exit 0;;


  

    


