open Unix;;
open OUnit;;
open Kyotocaml;;
open KCDb;;
open KCDb.Exc;;


module Cur = KCCur.Exc;;

let ( !!! ) = KCString.of_string
;;

let assert_equal_opt a = assert_equal (Some a)
;;

let cur:(KCCur.t option ref) = ref None
;;

let cur_else_skip () = match !cur with | Some c -> c | None -> skip_if true "No Cursor created"; raise (Failure "")
;;

let get_test_suite opendb name_of_suite = 
let db = make () in 
let ( >::. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal ec (ecode db) in
name_of_suite >:::[
	
	"open">:: (fun () -> assert_equal () (opendb db [KCOWRITER; KCOCREATE; KCOTRUNCATE]));
	"ecode">:: (fun () -> assert_equal 0 (ecode db));
	"version">::(fun () -> let ver = version () in assert_bool ver ("" <> ver));

	"add">::: [
	"0">::. (fun () -> assert_equal ()  (add_string db "Key0" "Value0"));
	"1">::. (fun () -> assert_equal () (add_string db "Key1" "Value1"));
	"2">::. (fun () -> assert_equal () (add_string db "Key2" "Value2"));
	"3">::. (fun () -> assert_equal () (add_string db "Key3" "Value3"));
	"4">::. (fun () -> assert_equal () (add db "Key4" (!!! "Value4")));
	"5">::. (fun () -> assert_equal () (add_string db "Key5" "Value5"));
	"6">::. (fun () -> assert_equal () (add_string db "Key6" "Value6"));
	"7">::. (fun () -> assert_equal () (add db "Key7" (!!! "Value7")));
	"8">::. (fun () -> assert_equal () (add_string db "Key8" "Value8"));
	"9">::. (fun () -> assert_equal () (add_string db "Key9" "Value9"));
	"10">::. (fun () -> assert_equal () (add_string db "Key10" "Value10"));
	"10.1">::. (fun () -> assert_equal () (add_string db "Key10" "Value10"));
	"11">::. (fun () -> assert_equal () (add_string db "Key11" "Value11"));
	"12">::. (fun () -> assert_equal () (add_string db "Key12" "Value12"));
	"13">::. (fun () -> assert_equal () (add db "Key13" (!!! "Value13")));
	"14">::. (fun () -> assert_equal () (add db "Key14" (!!! "Value14")));
	"15">::. (fun () -> assert_equal () (add db "Key15" (!!! "Value15")));
	];

"cursor">::: [
	"1">:: (fun () -> assert_equal  () (cur := Some(cursor db)));

	"2">::
	(fun () ->
		let c = cur_else_skip() in
		assert_equal () (Cur.jump c)
	);

	"3">::
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  true 
		(
			(Cur.getkey c false) = (!!!"0000000000000000") &&  
			(Cur.getvalue c false) = (!!!"Value0")
		)
	);

	"4">::
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  true ((Cur.getvalue c true) = (!!!"Value0")) 
	);
		
	"5">::
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  ("0000000000000007", "Value1") (Cur.get_as_string c true)
	);
		
	"6">::
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal (!!!"000000000000000E", !!!"Value2") (Cur.get c true)
	);

	"7">:: (fun () -> let c = cur_else_skip() in assert_equal  () (Cur.step c));

	"8">::
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  true ((Cur.getvalue c false) = (!!!"Value4"))
	);

	"9">::(fun () -> 
		let c = cur_else_skip() in
		assert_equal  () (Cur.jumpkey c  "0000000000000007" )
	);

	"10">:: 
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  ("0000000000000007", "Value1") (Cur.get_as_string c true)
	);

];

"iterate">::: [
	"1">:: (fun () -> let iter_func k v  o = o := Int64.add !o 1L; KCVISNOP in
		let icount = ref 0L in
		ignore(iterate db iter_func icount false); assert_equal 17L !icount);
];

"sync2">:: (fun () -> assert_equal () ((sync db true Nullproc ())));
"size">:: (fun () -> assert_bool "No size available" ((size db) > 0L));
"path">:: (fun () -> assert_bool "No path available" ((path db) <> ""));
"close">:: (fun () -> assert_equal () (close_db db));
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
	print_endline ">>> Modules: Kyotocaml.KCDb, Kyotocaml.KCDb.Exc,  Kyotocaml.KCCur.Exc <<<";
	let opendb = fun db -> open_text_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db "." "text_4" in
	let tst = (get_test_suite opendb "Text")  in
	run_test_tt_main (tst);
;;
