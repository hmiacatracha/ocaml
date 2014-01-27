
class ['a] vectorDinamico tam  (to_st:'a->string)= 
object (this)

val mutable numEltos = 0
val mutable buffer=Array.make tam None

method private from_option = function
        Some x -> x
        |None -> invalid_arg ("None_value")
        

method annade (n : 'a) = 
		if numEltos < (Array.length buffer) then
			begin
			buffer.(numEltos) <- Some n; 
			numEltos <- numEltos + 1
			end
		else
			begin
			buffer <- Array.append buffer (Array.make tam None);
			buffer.(numEltos) <- Some n;
			numEltos 	<- numEltos + 1
			end

method inserta p (n : 'a) = 
		if p >= numEltos then
			raise (Invalid_argument "index out of bounds")
		else
		if numEltos < (Array.length buffer) then
			begin
			for i = (Array.length buffer - 1) downto (p+1)
				do
				buffer.(i) <- buffer.(i-1)
				done;
			buffer.(p) <- Some n;
			numEltos <- numEltos + 1
			end
		else
			begin
			buffer <- Array.append buffer (Array.make tam None);
			for i = (Array.length buffer - 1) downto (p+1)
				do
				buffer.(i) <- buffer.(i-1)
				done;
			buffer.(p) <- Some n;
			numEltos <- numEltos + 1
			end	

method sobreescribe p (n : 'a) =
		if p >= numEltos then
			raise (Invalid_argument "index out of bounds")
		else
			buffer.(p) <- Some n
			
method longitud () = numEltos			

method elemento p = 
		if p >= numEltos then
			raise (Invalid_argument "index out of bounds")
		else
			buffer.(p)
method indice n = 
		let condicion = ref true in
		let ind = ref (-1) in
		let cont = ref 0 in
		while !condicion 
			do
			if ((!cont) < numEltos) && (buffer.(!cont) = n) then
				ind := !cont;
			if (!cont) = numEltos then
				condicion := false;
			cont := (!cont + 1)
			done;
		!ind	
method to_string ()=
		let acum = ref "[" in
		for i = 0 to (numEltos - 1)
			do
			if i < 1 then
				acum := (!acum ^ (to_st (this#from_option buffer.(i))))
			else
				acum := (!acum ^ "," ^ (to_st (this#from_option buffer.(i))))
			done;
		(!acum ^ "]")	
method to_stringBuffer ()=
		let acum = ref "[" in
		for i = 0 to (numEltos - 1)
			do
			if i < 1 then
				acum := (!acum ^ (to_st (this#from_option buffer.(i))))
			else
				acum := (!acum ^ "," ^ (to_st (this#from_option buffer.(i))))
			done;
		for i = (numEltos + 1) to (Array.length buffer)
			do
			if i < 2 then
				acum := (!acum ^ "_")
			else
				acum := (!acum ^ ",_")
			done;
		(!acum ^ "]")	
							
end;;

(*Vector dinamico para enteros *) 
class vectorDinamico_int tb =
 object 
   inherit ['int] vectorDinamico tb string_of_int
 end;;
