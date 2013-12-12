open Unix;;
open OUnit;;
open Kyotocaml;;
open KCDb;;

module E = Exc
;;

module R = Ret
;;

let opts = [KCOWRITER; KCOCREATE; KCOTRUNCATE]
;;

let ext = ".kch"
;;

let _ = 
let db = [|E.make (); E.make(); E.make()|] in
let dbmrg = E.make() in
let tx = "Exc">:: 
	bracket(
	fun () ->
		for i = 0 to (Array.length db) - 1 do
			let dbstr = string_of_int i in
			let db = Array.get db i in
			E.open_db db ("./dbx2merge" ^ dbstr ^ ext) opts; 
			for j = 1 to 100 do
				let s = (string_of_int j) ^ "x" ^ dbstr in
				E.add_string db ("Key" ^ s) ("Value" ^ s)
			done
		done;
		E.open_db dbmrg ("./dbx_merged" ^ ext) opts;
		(db, dbmrg))
		
		(fun (db, dbmrg) -> assert_equal () (E.merge dbmrg db KCMADD))
		
		(fun (db, dbmrg) ->
			Array.iter E.close_db db; 
			E.close_db dbmrg;
		)
in
let dopt = function | Some o -> o | None -> assert false in
let tr = "Ret">:: 
	bracket(
		fun () ->
		let db = [|dopt(R.make ()); dopt(R.make()); dopt(R.make())|] in
		let dbmrg = dopt(R.make()) in
		for i = 0 to (Array.length db) - 1 do
			let dbstr = string_of_int i in
			let db = Array.get db i in
			ignore(R.open_db db ("./dbr2merge" ^ dbstr ^ ext) opts);
			for j = 1 to 100 do
				let s = (string_of_int j) ^ "x" ^ dbstr in
				ignore(R.add_string db ("Key" ^ s) ("Value" ^ s))
			done
		done;
		ignore(R.open_db dbmrg ("./dbr_merged" ^ ext) opts);
		(db, dbmrg))

		(fun (db, dbmgr) -> assert_equal true (R.merge dbmrg db KCMSET))

		(fun (db, dbmgr) -> 
			Array.iter (fun db -> ignore(R.close_db db)) db; 
			ignore( R.close_db dbmrg );
		)
	
in
print_endline "Testing the merge functions of Kyotocaml.KCDb.Exc/Kyotocaml.KCDb.Ret";
let t = "merge">::: [tx; tr] in run_test_tt_main t;
Gc.full_major ()
;;

