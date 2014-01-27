class vectordinamico t =
object
	val tb = t
	val mutable numEltos = 0   
	val mutable buffer = Array.make t 0
	
	method annade n = 
		if numEltos < (Array.length buffer) then
			begin
			buffer.(numEltos) <- n;
			numEltos <- numEltos + 1
			end
		else
			begin
			buffer <- Array.append buffer (Array.make t 0);
			buffer.(numEltos) <- n;
			numEltos <- numEltos + 1
			end
	method inserta p n = 
		if p >= numEltos then
			raise (Invalid_argument "index out of bounds")
		else
		if numEltos < (Array.length buffer) then
			begin
			for i = (Array.length buffer - 1) downto (p+1)
				do
				buffer.(i) <- buffer.(i-1)
				done;
			buffer.(p) <- n;
			numEltos <- numEltos + 1
			end
		else
			begin
			buffer <- Array.append buffer (Array.make t 0);
			for i = (Array.length buffer - 1) downto (p+1)
				do
				buffer.(i) <- buffer.(i-1)
				done;
			buffer.(p) <- n;
			numEltos <- numEltos + 1
			end	
	method sobreescribe p n =
		if p >= numEltos then
			raise (Invalid_argument "index out of bounds")
		else
			buffer.(p) <- n
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
				acum := (!acum ^ (string_of_int buffer.(i)))
			else
				acum := (!acum ^ "," ^ (string_of_int buffer.(i)))
			done;
		(!acum ^ "]")
	method to_stringBuffer ()=
		let acum = ref "[" in
		for i = 0 to (numEltos - 1)
			do
			if i < 1 then
				acum := (!acum ^ (string_of_int buffer.(i)))
			else
				acum := (!acum ^ "," ^ (string_of_int buffer.(i)))
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
