open Kyotocaml.KCDb;;
open Kyotocaml.KCDb.Ret;;

module Cur = Kyotocaml.KCCur;;

let to_string = Kyotocaml.KCString.to_string;;

let mkkeyval i =
	let soi = (string_of_int i) in
	"K" ^ soi, "K" ^ soi
;;
	
let test () =
let db_fname = "ex1db" in
print_string "=========== Test 1 ==========\n";
print_string "Kyoto Cabinet Lib Version: "; print_endline (Kyotocaml.version ());
let dbo = make () in
match dbo with
|None -> Printf.fprintf stderr "%s\n" "Couldn't create Databaseoobject"; exit (-1)
|Some db ->
	if false = (open_file_hash_db db "./" db_fname [KCOWRITER; KCOREADER; KCOCREATE]) then
		Printf.fprintf stderr "%s\n%!" ("\t**DB " ^ db_fname ^ " could not be created/opened")
	else
		ignore(clear db);
		let n = 200000 in
		Printf.printf "\n\n\tAdding %d key-value pairs to database: wait a moment...\n\n%!" n;
		let ti = Sys.time () in
		for i = 1 to n do
			let key, value = mkkeyval i in
			ignore(add_string db key value)
		done;
		Printf.printf "\tElapsed time: %g s\n\n%!" ((Sys.time ()) -. ti); 
		Random.init 17;
		let m = 200 in
		Printf.printf "\tQuery about %d randomly chosen keys (ca.10%% mismatch expected)\n%!" m;
		for i = 1 to m do
			let nn = n + n / 10 in
			let key = "K" ^ (string_of_int (1 + (Random.int nn))) in
			let value = get_as_string db key in
			match value with
			|Some s -> Printf.printf "\t%-3d: Key=%s : Value=%s\n%!" i key s
			|None -> Printf.printf "\t%-3d: Key=%s : Not found\n%!" i key
		done;
		for i = n + 1 to n + 100 do
			let key, value = mkkeyval i in
			ignore(add_string db key value)
		done;
		Printf.printf "\n\n\tQuery key-value pairs by Cursor-iteration\n%!";
		let crsr_opt = cursor db in
		match crsr_opt with
		| None -> Printf.fprintf stderr "\t**Cannot open cursor\n%!";
		| Some crsr -> 
			if false = (Cur.Ret.jump crsr) then Printf.fprintf stderr "\t**KCCur.jump failed (Emsg: %s)\n%!" (emsg db) else 
			begin
				for i = 1 to (n/1000) do
					let kv_opt = Cur.Ret.get crsr true in
					match kv_opt with
					|None -> Printf.fprintf stderr "\t**KCCur.get failed\n%!"
					|Some (k,v) -> Printf.printf "\tKey=%s : Value=%s\n%!" (to_string k) (to_string v); ignore(Cur.Ret.step crsr)
				done;
				if false = (Cur.Ret.jumpback crsr) then Printf.fprintf stderr "\t**KCCur.jumpback failed (emsg: %s)\n%!" (emsg db) else
				begin
					for i = 1 to (n/1000) do
					let kv_opt = Cur.Ret.get crsr true in
					match kv_opt with
					|None -> Printf.fprintf stderr "\t**KCCur.get failed\n%!"
					|Some (k,v) -> Printf.printf "\tKey=%s : Value=%s\n%!" (to_string k) (to_string v); ignore(Cur.Ret.stepback crsr)
				done;
			end;
		end;
		(match status db with
		| Some st -> Printf.printf "\nStatus:\n%s\n" st
		| None -> Printf.fprintf stderr "\t**No status");
		ignore(close_db db);
		Printf.printf "Used functions:\n\tKCDb.Ret: open_db, close_db, add_string, cursor\n";
		Printf.printf "\tKCCur.Ret: jump, get, jumpback, step\n\tKCDb: emsg, status%!"; 
		Printf.printf "\n\n********** Test finished **********\n"
;;

let _ = test ()
;;