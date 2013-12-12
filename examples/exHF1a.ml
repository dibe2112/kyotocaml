open Kyotocaml.KCDb;;
open Kyotocaml.KCDb.Ret;;

module KCS = Kyotocaml.KCString;;

let to_string = KCS.to_string;;

let test () =
let db_fname = "ex1db" in
print_string "=========== Test 1a ==========\n";
print_string "Kyoto Cabinet Lib Version: "; print_endline (Kyotocaml.version ());
let dbo = make () in
match dbo with
|None -> Printf.fprintf stderr "%s\n" "Couldn't create Databaseoobject"; exit (-1)
|Some db ->
	if false = (open_file_hash_db db "./" db_fname [KCOWRITER; KCOREADER]) then begin
		Printf.fprintf stderr "%s\n" ("Database " ^ db_fname ^ " could not be opened");
		Printf.fprintf stderr "\nDid you run test1hf.byte first to create Database?\n%!";
		exit(200)
	end else
		let count_records = count db in
		let db_size = size db in
		Printf.printf "Opened Database %s with %Ld records and a size of %Ld bytes\n\n" (path db) count_records db_size;
		if count_records < 1L then begin
			Printf.fprintf stderr "No records in database!!!\n\nTest aborted\n";
			ignore(close_db db);
			exit (-2)
		end;
		Printf.printf "iterate 1st run:\n%!";
		let iterf k v  o =
			if (!o mod 10000) = 0 || !o = 1 then Printf.printf " %s|%s %!"  k v;
			incr o;
			KCVISNOP 
		in
		let ti1 = Sys.time () in
		let icount = ref 1 in
		ignore(iterate_string db iterf icount false);
		Printf.printf "\nElapsed time: %g s\n" ((Sys.time ()) -. ti1); 
		Printf.printf  "Iterated records: %d\n\n" (pred(!icount));
		let iterf2 k v  o =
			if (!o mod 10000) = 0 || !o = 1 then Printf.printf " %s|%s %!" (to_string k) (to_string v);
			incr o;
			KCVISNOP 
		in
		Printf.printf  "Iterated records: %d\n\n" (pred(!icount));
		Printf.printf "iterate 2nd run:\n%!";
		let ti2 = Sys.time () in
		let icount = ref 1 in
		ignore(iterate db iterf2 icount false);
		Printf.printf "\nElapsed time: %g s\n" ((Sys.time ()) -. ti2); 
		Printf.printf  "Iterated records: %d\n\n" (pred(!icount));
		let iterf3 k v  o =
			if (!o mod 10000) = 0 || !o = 1 then Printf.printf " %s|%s %!" (to_string k) (to_string v);
			let v' = KCS.copy v in
			let c = KCS.get v' 0 in
			let c' = if c = 'V' then 'v' else 'V' in
			KCS.set v' 0 c';
			incr o;
			KCVISRESULT v'
		in
		Printf.printf "iterate 3rd run (changes first letter of value):\n%!";
		let ti3 = Sys.time () in
		let icount = ref 1 in
		ignore(iterate db iterf3 icount false);
		Printf.printf "\nElapsed time: %g s\n" ((Sys.time ()) -. ti3); 
		Printf.printf  "Iterated records: %d\n\n" (pred(!icount));
		Printf.printf "iterate 3rd run:\n%!";
		Printf.printf "iterate repeat 2nd run; 1st letter changed:\n%!";
		let icount = ref 1 in
		ignore(iterate db iterf2 icount false);
		(match status db with
		| Some st -> Printf.printf "\n\nStatus:\n%s\n" st
		| None -> print_endline "No status");
		ignore(close_db db);
		Printf.printf "\n\n********** Test finished **********\n"
;;

let _ = test ()
;;