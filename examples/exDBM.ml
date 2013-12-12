open Kcdbm;;

module H = Hash;;
module T = Tree;;

let _ = Random.init 1;;

let _ = Random.init 1;;

let counter = ref 0;;

let iterfunc k _ = if k.[3] = '1' then incr counter;;

let runH () = 
  let dbfile = "./hdbm" in
  begin try Sys.remove (dbfile ^ ".kch") with _ -> () end;
  Printf.printf "Try to open hash database\n%!";
  let db = H.opendbm dbfile [H.Dbm_rdwr; H.Dbm_create] 0o644 in
  Printf.printf "Adding 100 records to hash database: %!";
  for i = 1 to 100 do
	let k = "Key" ^ (string_of_int i) in
	let v = "Value" ^ (string_of_int i) in
	Printf.printf ".%!";
	try H.add db k v with H.Dbm_error("Entry already exists") -> print_endline "Entry already exists"
  done;
  Printf.printf "\n%!";
  Printf.printf "Searching for 20 randomly chosen keys (some may not be found)\n%!";
  for i = 1 to 20 do
	let k = "Key" ^ (string_of_int(Random.int 120)) in
	try Printf.printf "Key=%s ... found value %s\n%!" k (H.find db k) 
	with | _ -> Printf.printf "Key=%s ... fail\n%!" k
  done;
  Printf.printf "Iterating (counting records  with key.[3] = '1')...\n%!";
  H.iter iterfunc db;
  Printf.printf "Result=%d\n%!" !counter;
  Printf.printf "firstkey ... %s\n%!" (H.firstkey db);
  Printf.printf "nextkey ... %s\n%!" (H.nextkey db);
  Printf.printf "nextkey ... %s\n%!" (H.nextkey db);
  Printf.printf "nextkey ... %s\n%!" (H.nextkey db);
  Printf.printf "nextkey ... %s\n%!" (H.nextkey db);
  begin
   let rest = ref 0 in
	try
		while true do
			ignore(H.nextkey db);
         incr rest
		done
	with
   |Not_found -> Printf.printf "Rest count=%d\n%!" !rest
  end;
  for i = 1 to 2 do 
  try H.remove db "Key25"; print_endline  "Key25 removed" 
  with | H.Dbm_error "dbm_store failed" -> 
     if i = 1 then
        print_endline "Error: removing Key25"
     else
        print_endline "Key25 connot be removed a second time ... OK";
  done;
  begin
  try H.replace db "Key15" "Value15replaced"; print_endline  "Key15: Value replaced" 
  with | H.Dbm_error "dbm_store failed" -> print_endline "Error: replacing value for Key15"; 
  end;
  begin
  try Printf.printf "New value for Key15 ... found %s\n%!" (H.find db "Key15") 
  with | _ -> Printf.printf "find Key15 ... fail\n%!"
  end;
  begin
  try H.replace db "Key250" "Value250"; print_endline  "Key250: Value replaced" 
  with | H.Dbm_error "dbm_store failed" -> print_endline "Error: replacing value for Key250"; 
  end;
  H.close db;
  Gc.full_major();
;;

let runT () = 
  let dbfile = "./tdbm" in
  begin try Sys.remove (dbfile ^ ".kct") with _ -> () end;
  Printf.printf "Try to open tree database\n%!";
  let db = T.opendbm dbfile [T.Dbm_rdwr; T.Dbm_create] 0o644 in
  Printf.printf "Adding 100 records to tree database: %!";
  for i = 1 to 100 do
	let k = "Key" ^ (string_of_int i) in
	let v = "Value" ^ (string_of_int i) in
	Printf.printf ".%!";
	try T.add db k v with T.Dbm_error("Entry already exists") -> print_endline "Entry already exists"
  done;
  Printf.printf "\n%!";
  Printf.printf "Searching for 20 randomly chosen keys (some may not be found)\n%!";
  for i = 1 to 20 do
	let k = "Key" ^ (string_of_int(Random.int 120)) in
	try Printf.printf "Key=%s ... found value %s\n%!" k (T.find db k) 
	with | _ -> Printf.printf "Key=%s ... fail\n%!" k
  done;
  counter := 0;
  Printf.printf "Iterating (counting records with key.[3] = '1')...\n%!";
  T.iter iterfunc db;
  Printf.printf "Result=%d\n%!" !counter;
  Printf.printf "Firstkey ... %s\n%!" (T.firstkey db);
  Printf.printf "nextkey ... %s\n%!" (T.nextkey db);
  Printf.printf "nextkey ... %s\n%!" (T.nextkey db);
  Printf.printf "nextkey ... %s\n%!" (T.nextkey db);
  Printf.printf "nextkey ... %s\n%!" (T.nextkey db);
   begin
   let rest = ref 0 in
	try
		while true do
			ignore(T.nextkey db);
         incr rest
		done
	with
   |Not_found -> Printf.printf "Rest count=%d\n%!" !rest
  end;
  for i = 1 to 2 do
  try T.remove db "Key25"; print_endline  "Key25 removed" 
  with | T.Dbm_error "dbm_store failed" -> 
     if i = 1 then
        print_endline "Error: removing Key25"
     else
        print_endline "Key25 connot be removed a second time ... OK";
  done;
  begin
  try T.replace db "Key15" "Value15replaced"; print_endline  "Key15: Value replaced" 
  with | T.Dbm_error "dbm_store failed" -> print_endline "Error: replacing value for Key15"; 
  end;
  begin
  try Printf.printf "New value for Key15 ... found %s\n%!" (T.find db "Key15") 
  with | _ -> Printf.printf "find Key15 ... fail\n%!"
  end;
  begin
  try T.replace db "Key250" "Value250replaced"; print_endline  "Key250: Value replaced" 
  with | T.Dbm_error "dbm_store failed" -> print_endline "Error: replacing value for Key250"; 
  end;
  T.close db;
  Gc.full_major();
;;

let _ = 
runH (); runT ()
;;
