open Unix;;
open OUnit;;
open Kyotocaml;;
open KCDb;;

module R = KCDb.Ret;;
module X = KCDb.Exc;;


let ( !!! ) = KCString.of_string
;;

let assert_equal_opt a = assert_equal (Some a)
;;

let klist = [("BK1"); ("BK2"); ("BK3"); ("BK4")];;

let kvlist0 = [("BK1", !!!"BV1"); ("BK2", !!!"BV2"); ("BK3", !!!"BV3"); ("BK4", !!!"BV4")];;

let kvlist1 = [("BK1", "BV1"); ("BK2", "BV2"); ("BK3", "BV3"); ("BK4", "BV4")];;


let get_test_suite opendb name_of_suite = 
let db = match R.make () with 
	| Some db -> db 
	| None -> prerr_endline "Fatal error: Cannot create database objekt\nTest is terminated"; exit (-1) in 
name_of_suite >:::[
	
"open">:: (fun () -> assert_equal true (opendb db [KCOWRITER; KCOCREATE; KCOTRUNCATE]));
"ecode">:: (fun () -> assert_equal 0 (ecode db));

"setbulk">::(fun() -> assert_equal_opt 4 (R.setbulk db kvlist0 false));
"count1">::(fun() -> assert_equal 4L (count db));
"removebulk">::(fun() -> assert_equal_opt 2 (R.removebulk db ["BK1"; "BK4"; "BK8"] false));
"getbulk">::(fun() -> let lropt = R.getbulk db klist false in
            match lropt with 
				| Some lr -> assert_equal 2 (List.length lr) 
				| _ -> assert_equal 1 2); 


"count1">::(fun() -> assert_equal 2L (count db));

"setbulk_string">::(fun() -> ignore(R.clear db); assert_equal_opt 4 (R.setbulk_string db kvlist1 true));
"getbulk_as_string">::(fun() -> let lropt = R.getbulk_as_string db klist false in
			match lropt with 
				| Some lr -> assert_equal 4 (List.length lr) 
				| _ -> assert_equal 1 2); 
"count2">::(fun() -> assert_equal 4L (count db));

"setbulk:x">::(fun() -> ignore(X.clear db); assert_equal 4 (X.setbulk db kvlist0 false));
"count3">::(fun() -> assert_equal 4L (count db));
"removebulk:x">::(fun() -> assert_equal 2 (X.removebulk db ["BK1"; "BK4"; "BK8"] false));
"getbulk:x">::(fun() -> let lr = X.getbulk db klist false in
			assert_equal 2 (List.length lr)); 


"count4">::(fun() -> assert_equal 2L (count db));

"setbulk_string:x">::(fun() -> X.clear db; assert_equal 4 (X.setbulk_string db kvlist1 true));
"getbulk_as_string:x">::(fun() -> let lr = X.getbulk_as_string db klist false in
				assert_equal 4 (List.length lr)); 

"count2">::(fun() -> assert_equal 4L (count db));

"close">:: (fun () -> assert_equal true (R.close_db db));
"GC">:: (fun () -> assert_equal () (Gc.full_major()));

];;


let _ = 
	print_endline "\n*** Delete old test data... ***";
	if 0 <> Sys.command "make cleandata" then begin
		print_endline "Unable to delete old test data.\nTest may deliver wrong results";
		print_string "Continue? [Yn] ";
		if (read_line ()) <> "Y" then exit 0
	else
		print_newline ();
	end;
	
	print_endline ">>> Test Bulk operation <<<";
	print_endline ">>> Modules: Kyotocaml.KCDb, Kyotocaml.KCDb.Ret/Exc <<<";
	let opendb = fun db -> R.open_file_hash_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db "." "Bulk" in
	let tst = (get_test_suite opendb "Bulk")  in
	run_test_tt_main (tst);
;;
