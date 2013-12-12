open Unix;;
open OUnit;;
open Kyotocaml;;
open KCDb;;
open KCDb.Ret;;


module Cur = KCCur.Ret;;

let ( !!! ) = KCString.of_string
;;

let assert_equal_opt a = assert_equal (Some a)
;;

let cur = ref None
;;

let skip_if_no_cur () = skip_if (!cur = None) "No Cursor created"
;;

let dopt (Some o) = o
;;

let get_test_suite opendb name_of_suite = 
let db = match make () with 
	| Some db -> db 
	| None -> prerr_endline "Fatal error: Cannot create database objekt\nTest is terminated"; exit (-1) in 
let ( >::. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal ec (ecode db) in
name_of_suite >:::[
	
	"open">:: (fun () -> assert_equal true (opendb db [KCOWRITER; KCOCREATE; KCOTRUNCATE]));
	"ecode">:: (fun () -> assert_equal 0 (ecode db));
	"version">::(fun () -> let ver = version () in assert_bool ver ("" <> ver));

	"add">::: [
	"0">::. (fun () -> assert_equal true  (add_string db "Key0" "Value0"));
	"1">::. (fun () -> assert_equal true (add_string db "Key1" "Value1"));
	"2">::. (fun () -> assert_equal true (add_string db "Key2" "Value2"));
	"3">::. (fun () -> assert_equal true (add_string db "Key3" "Value3"));
	"4">::. (fun () -> assert_equal true (add db "Key4" (!!! "Value4")));
	"5">::. (fun () -> assert_equal true (add_string db "Key5" "Value5"));
	"6">::. (fun () -> assert_equal true (add_string db "Key6" "Value6"));
	"7">::. (fun () -> assert_equal true (add db "Key7" (!!! "Value7")));
	"8">::. (fun () -> assert_equal true (add_string db "Key8" "Value8"));
	"9">::. (fun () -> assert_equal true (add_string db "Key9" "Value9"));
	"10">::. (fun () -> assert_equal true (add_string db "Key10" "Value10"));
	"10.1">::. (fun () -> assert_equal true (add_string db "Key10" "Value10"));
	"11">::. (fun () -> assert_equal true (add_string db "Key11" "Value11"));
	"12">::. (fun () -> assert_equal true (add_string db "Key12" "Value12"));
	"13">::. (fun () -> assert_equal true (add db "Key13" (!!! "Value13")));
	"14">::. (fun () -> assert_equal true (add db "Key14" (!!! "Value14")));
	"15">::. (fun () -> assert_equal true (add db "Key15" (!!! "Value15")));
	];

"cursor">::: [
	"1">:: (fun () -> assert_equal  true (match cursor db with | Some c -> cur := Some c; true | None -> false ));

	"2">::
	(fun () -> skip_if_no_cur (); let c = dopt !cur in assert_equal  true (Cur.jump c) );

	"3">::
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  true 
		(
			(Cur.getkey c false) = (Some(!!!"0000000000000000")) &&  
			(Cur.getvalue c false) = (Some(!!!"Value0"))
		)
	);

	"4">::
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  true ((Cur.getvalue c true) = (Some(!!!"Value0"))) 
	);
		
	"5">::
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  (Some("0000000000000007", "Value1")) (Cur.get_as_string c true)
	);
		
	"6">::
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		(*print_endline (dopt(Cur.getkey_as_string c false));*)
		assert_equal  (Some(!!!"000000000000000E", !!!"Value2")) (Cur.get c true)
	);

	"7">:: (fun () -> skip_if_no_cur(); assert_equal  true (let c = dopt !cur in Cur.step c));

	"8">::
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  true ((Cur.getvalue c false) = (Some(!!!"Value4")))
	);

	"9">::(fun () -> skip_if_no_cur(); assert_equal  true (let c = dopt !cur in Cur.jumpkey c  "0000000000000007" ));

	"10">:: 
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  (Some("0000000000000007", "Value1")) (Cur.get_as_string c true)
	);

];

"iterate">::: [
	"1">:: (fun () -> let iter_func k v  o = o := Int64.add !o 1L; KCVISNOP in
		let icount = ref 0L in
		ignore(iterate db iter_func icount false); assert_equal 17L !icount);
];

"sync2">:: (fun () -> assert_bool "sync" ((sync db true Nullproc ())));
"size">:: (fun () -> assert_bool "No size available" ((size db) > 0L));
"path">:: (fun () -> assert_bool "No path available" ((path db) <> ""));
"close">:: (fun () -> assert_equal true (close_db db));
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
	
	print_endline ">>> Test Text databases <<<";
	print_endline ">>> Modules: Kyotocaml.KCDb, Kyotocaml.KCDb.Ret,  Kyotocaml.KCCur.Ret <<<";
	let opendb = fun db -> open_text_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db "." "text_4" in
	let tst = (get_test_suite opendb "Text")  in
	run_test_tt_main (tst);
;;
