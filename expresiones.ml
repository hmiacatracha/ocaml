(**
  NOMBRE:  HMIA
**)	  

(**HACEMOS REFERENCIA A RESULTADO DE OPERACIONES REPETIDAS PORQUE ME DABAN ERROR**)
();;
(*-:unit=() *)

2+5*3;;
(*-:int=17*)

1.0;;
(*-:float=1.0*)

(*1.0*2*)
(* Error: This expression has type float but an expression was expected of type int *)
(*Error de tipo léxico, el operador * indica que los valores a introducir deben de ser del mismo tipo, en este caso este operador indica que es de tipo entero (int), por lo que la forma correcta es la siguiente:*)
1*2;;
(* - : int = 2 *)

(*2-2.0;;*)
(* Error: This expression has type float but an expression was expected of type int *)
(*Error de tipo léxico, el operador - indica que los valores a introducir deben de ser del mismo tipo, en este caso este operador indica que es de tipo entero (int), y no float como 2.0, por lo que la forma correcta es la siguiente*)
2-2;;
(**-:int=0**)

(*3.0+2.0;;*)
(* Error: This expression has type float but an expression was expected of type int *)
(*Error de tipo léxico, en este caso 3.0 y 2.0 si que tienen el mismo tipo, el problema es que el operador + no hace referencia a este tipo de datos, sino que opera con datos enteros(int): la forma correcta sería la siguiente*)
3+2;;
(**-:int=5**)

3.0 +. 2.0;;
(*-:float=5.*)

5/3;;
(*-:int=1*)

5 mod 3;;
(*-:int=2*)

3.0*.2.0**3.0;;
(*-:float=24.0*)

3.0=float_of_int 3;;
(*-:bool=true*)

(*sqrt 4;;*)
(* Error: This expression has type int but an expression was expected of type float *)
(*Error léxico, La funcion sqrt su parámetro tiene que ser de tipo float. La forma es la siguiente:*)
sqrt 4.;;
(**-:float = 2. **)

int_of_float 2.1+int_of_float(-2.9);;
(*-:int:0*)

truncate 2.1+truncate (-2.9);;
(*-:int=0*)

floor 2.1 +. floor (-2.9);;
(* -: float = -1.0 *)

(*ceil 2.1+ceil -2.9;;*)
(* Error: This expression has type float -> float but an expression was expected of type float *)
(*Error sintático , Se espera de tipo float el ceil -2.9 no lo identifica porque nos olvidamos del parentésis*)
ceil 2.1 +. ceil (-2.9);;
(**-:float =1.**)

'B';;
(*:-char='B'*)

int_of_char 'A';;
(*-:int=65*)

char_of_int 66;;
(*-:char='B'*)

Char.code 'B';;
(*-:int=66*)

Char.chr 67;;
(*-:char='C'*)

'\067';;
(*-:char='C'*)

(*Char.chr(Char.code 'a'-Char.code 'A'+Char.code'Ñ';;*)
(*Syntax error: ')' expected, the highlighted '(' might be unmatched*)
(*Error de sintático, errores tipográficas, paréntesis sin cerrar, etc. En este caso es paréntesis sin cerrar. RECORDEMOS CAMBIARLE LA CODIFICACION DE CARÁCTERES A OCCIDENTAL(ISO-8859-15) QUE ACEPTA LA LETRA Ñ, SINO NOS PODRÍA DAR ERROR *)
(*Char.chr(Char.code 'a'-Char.code 'A'+ Char.code 'Ñ');;*)
(**-:char='\241'**)

(*Char.uppercase 'ñ;;*)
(* Error: Syntax error *)
(* Error de tipo sintático, se nos ha olvidado cerrrar las comilla simple de la ñ*)
(**RECUERDER CAMBIARLE LA CODIFICACION DE CARACTERES A OCCIDENTAL(ISO-8859-15, SINO DARÁ ERROR**)
(*Char.uppercase 'ñ';;*)
(**- : char = '\209'**)

Char.uppercase 'n';;
(*-:char='N'*)

Char.lowercase 'O';;
(*-:char='o'*)

"this is a string";;
(*-:string="this is a string"*)

String.length "longitud";;
(*:-int=8*)

(*"1999"+"1";;*)
(* Error: This expression has type string but an expression was expected of type int *)
 (*Error léxico, el operador + indica que los valores que pueden utilizar este operador, deben ser valores de tipo enteros(int), en este caso son cadenas por lo que los convertimos a numeros asi:*)
int_of_string "1999" + int_of_string "1";;
(**-:int=2000**)

"1999"^"1";;
(*-:string="19991"*)

int_of_string "1999"+1;;
(*-:int=2000*)

"\064\056";;
(*-:char="@8"*)

string_of_int 010;;
(*-:string="10"*)

not true;;
(*-:bool=false*)

true&&true;;
(*-:bool=true*)

true||false;;

(*-:bool=true*)
true or false;;

(*true and false;*)
(* Error: Syntax error *)
(*Error sintático, el operador and no existe como palabra propia del lenguaje de programación, en caso de que queremos hacer el and utilizamos el &&, esto si tiene significado dentro del lenguaje de programación, la forma sería la siguiente:.*)
true &&false;;
(**-:bool=false**)

(1<2)=false;;
(*-:bool=false*)

"1"<"2";;
(*-:bool=true*)

2<12;;
(*-:bool=true*)

"2"<"12";;
(*-:bool=false*)

"uno"<"dos";;
(*-:bool=false*)

2,5;;
(*-:int*int=2,5*)

"hola", "adios";;
(*-:sting*string="hola","adios"*)

0,0.0;;
(*-:int*float=0,0.0*)

fst('a',0);;
(*-:char='a'*)

snd(false,true);;
(*-:bool=true*)

(1,2,3);;
(*int*int*int=(1,2,3)*)

(1,2),3;;
(*int*int)int=(1,2),3*)

fst((1,2),3);;
(*-:int*int=(1,2)*)

(),abs;;
(*:-unit*(int->int)=((),<fun>*)

if 3=4 then 0 else 4;;
(*-:int=4*)

(*if 3=4 then 0 else "4";;*)
(* Error: This expression has type string but an expression was expected of type int *)
(*Error léxico, una funcion no puede devolver varios tipos, en ocaml está establecido que la funcion tiene que devolver un  solo tipo, ejemplo: *) 
if 3=4 then "0" else "4";;
(**- : string = "4"**)
if 3=4 then 0 else 4;;
(** - : int = 4 **)

(if 3<5 then 8 else 10)+4;;
(*int=12*)

let pi=3.14;;
(*val pi:float=3.14*)

sin(pi/.2.);;
(*-float=0.99999968293183461*)

let x=1;;
(*val x:int=1*)

let y=2;;
(*val y:int=2*)

x-y;;
(*-:int=-1*)

(*z;;*)
(* Error: Unbound value z *)
(*error de ejecucion, no existe z, aun no lo hemos creado*)

let z=x+y;;
(*val:z=3*)

z;;
(*-:int=3*)

let x=5;;
(*val x:int=5*)

z;;
(*-:int=3*)

let y=5 in x+y;;
(*-:int=10*)

x+y;;
(*-:int=7*)

let p=2,5;;
(*val p:int*int=(2,5)*)

snd p, fst p;;
(*-:int*int=(5,2)*)

p;;
(*-:int*int=(2,5)*)

let p=0,1 in snd p, fst p;;
(*val p:int*int=1,0*)

p;;
(*-:int*int=(2,5)*)

let x,y=p;;
(* 	
val x:int=2
val y:int=5
*)


let z=x+y;;
(*val z:int=7*)

let x,y=p,x;;
(*
 val x:int*int=(2,5)
val y:int=2
*)

let x=let x,y=2,3 in x* x+y;;
(*val x : int = 7*)

x+y;;
(*-:int = 9 *)

z;;
(*-:int=7*)

let x=x+y in let y=x*y in x+y+z;;
(*- : int = 34 *)

x+y+z;;
(*-:int=16*)

int_of_float;;
(*-:float->int<fun>*)

float_of_int;;
(*-:int->float<fun>*)

int_of_char;;
(*-:char->int<fun>*)

char_of_int;;
(*-:int->char<fun>*)

abs;;
(*- : int -> int = <fun>*)

sqrt;;
(*-:float->float=<fun>*)

truncate;;
(*-:float->int=<fun>*)

ceil;;
(*-:float->float=<fun>*)

floor;;
(*-:float->float=<fun> *)

Char.code;;
(*-:char->int<fun>*)

String.length;;
(*-:string->int<fun>*)

fst;;
(*- : 'a * 'b -> 'a = <fun>*)

snd;;
(*-:'a*'b->'b=<fun>*)

function x -> 2 * x;;
(* -:int->int = <fun> *)

(function x->2*x)(2+1);;
(*-:int=6*)

function (x,y)->x;;
(*-:'a*'b->'a=<fun>*)

let f=function x->2*x;;
(*val f : int -> int = <fun>*)

f (2+1);;
(*-:int=6*)
(*La funcion f con x=2+1 *)

f 2+1;;
(*-:int=5*)
(*La funcion f con x=2 y luego se le suma 1 *)






















