open Unix;;
open OUnit;;
open Kyotocaml;;
open KCDb;;
open Ret;;

module Cur = KCCur.Ret;;

let ( !!! ) = KCString.of_string
;;

let assert_equal_opt a = assert_equal (Some a)
;;

let cur = ref None
;;

let skip_if_no_cur () = skip_if (!cur = None) "No Cursor created"
;;

let openopt_rwc = [KCOWRITER; KCOCREATE; KCOTRUNCATE]
;;

let dopt (Some o) = o
;;

let keynr a = Scanf.sscanf (KCString.to_string (dopt a)) "Key%d" (fun n -> n)
;;

let valuenr a = Scanf.sscanf (KCString.to_string (dopt a)) "Value%d" (fun n -> n)
;;

let get_test_suite opendb name_of_suite openopt is_hash = 
let db = match make () with 
	| Some db -> db 
	| None -> prerr_endline "Fatal error: Cannot create database objekt\nTest is terminated"; exit (-1) in 
let ( >::. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal ec (ecode db) in
let ( >::.. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal true (ec <> (ecode db)) in
let skip_if_hash () = skip_if is_hash "Not implemented" in
name_of_suite >:::[
	"new db">:: (fun () -> assert_equal () (Printf.printf "\n%!"));
	"open">:: (fun () -> assert_equal true (opendb db openopt));
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
	"10a">::.. (fun () -> assert_equal false (add_string db "Key10" "Value10"));
	"11">::. (fun () -> assert_equal true (add_string db "Key11" "Value11"));
	"12">::. (fun () -> assert_equal true (add_string db "Key12" "Value12"));
	"13">::. (fun () -> assert_equal true (add db "Key13" (!!! "Value13")));
	"14">::. (fun () -> assert_equal true (add db "Key14" (!!! "Value14")));
	"15">::. (fun () -> assert_equal true (add db "Key15" (!!! "Value15")));
];
	
"check">::: [
	"1">::. (fun () -> assert_equal_opt 6 (check db "Key1"));
	"2">:: (fun () -> assert_equal None (check db "xxx"));
	"3">:: (fun () -> assert_equal_opt 7 (check db "Key10"));
];

"cursor">::: [
	"1">:: (fun () -> assert_equal  true (match cursor db with | Some c -> cur := Some c; true | None -> false ));

	"2">:: 
	(fun () -> skip_if_no_cur (); let c = dopt !cur in assert_equal  true (Cur.jump c) );

	"3">:: 
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  true (keynr(Cur.getkey c false) = valuenr(Cur.getvalue c false))
	);
		
	"4">:: 
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  true (let n = keynr(Cur.getkey c false) in n = valuenr(Cur.getvalue c true)) 
	);
		
	"5">:: 
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal  ("Key", "Value") (let (k, v) = dopt(Cur.get_as_string c false) in (String.sub k 0 3, String.sub v 0 5))
	);
		
	"6">::
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in
		assert_equal true (let (k,v) = dopt(Cur.get c false) in keynr(Some k) = valuenr(Some v))
	);
		
	"7">:: 
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in   
		assert_equal true (let (k,v) = dopt(Cur.get c true) in keynr(Some k) = valuenr(Some v))
	);
		
	"8">::
	(fun () -> 
		skip_if_no_cur(); 
		let c = dopt !cur in
		assert_equal true (let (k,v) = dopt(Cur.get c true) in keynr(Some k) = valuenr(Some v))
	);
		
	"9">::(fun () -> skip_if_no_cur(); assert_equal  true (let c = dopt !cur in Cur.jumpkey c  "Key2" ));
	"10">:: (fun () -> skip_if_no_cur(); assert_equal  true (let c = dopt !cur in Cur.step c));
	"11">::(fun () -> skip_if_no_cur(); skip_if_hash (); let c = dopt !cur in assert_equal  true (Cur.jumpback c ));
	
	"12">:: 
	(fun () -> 
		skip_if_no_cur (); 
		let c = dopt !cur in   
			assert_equal true (let (k,v) = dopt(Cur.get c true) in (keynr(Some k) = valuenr(Some v)))
	);

	"13">:: 
	(fun () -> 
		skip_if_no_cur (); 
		skip_if_hash (); 
		assert_equal  true (let c = dopt !cur in Cur.stepback c);
	);

	"14">:: 
	(fun () -> skip_if_no_cur (); 
		let c = dopt !cur in 
		Cur.jump c;
		let v = !!! "Cur.setvalue#" in
		assert_equal true (Cur.setvalue c v true)
	);

	"15">::
	(fun () -> skip_if_no_cur (); 
		let c = dopt !cur in 
		let v = "Cur.setvalue##" in
		assert_equal true (Cur.setvalue_string c v false)
	);

	"16">::
	(fun () -> skip_if_no_cur (); 
		let c = dopt !cur in 
		let v = "Cur.setvalue##" in
		let vopt = Cur.getvalue_as_string c false in
		match vopt with | None -> print_string "none" | Some vr -> print_string vr;
		assert_equal (Some v) vopt
	);

	"17">:: 
	(fun () -> skip_if_no_cur (); 
		let c = dopt !cur in 
		Cur.jump c;
		let v = !!! "Cur.setvalue#" in
		let vopt = Cur.getvalue c false in
		(*match vopt with | None -> print_string "none" | Some vr -> print_string (KCString.to_string vr);*)
		assert_equal (Some v) vopt
	);
];

"append">::: [
	"1">:: (fun () -> assert_equal true (append_string db "Key1" "+append_string"));
	"2">:: (fun () -> assert_equal true (append db "Key2" (!!! "+append")));
	"3">:: (fun () -> assert_equal false (add_string db "Key2" "+add_string"));
	"4">:: (fun () -> assert_equal true (append_string db "Key11" "+append_string"));
];
	
"get">::: [
	"1">:: (fun () -> assert_equal_opt "Value5" (get_as_string db "Key5"));
	"2">:: (fun () -> assert_equal_opt (!!! "Value5") (get db "Key5"));
	"3">:: (fun () -> assert_equal_opt (!!! "Value11+append_string") (get db "Key11"));
	"4">:: (fun () -> assert_equal None (get_as_string db "UnusedKey"));
	"5">:: (fun () -> assert_equal ("Value", 6) ( let s = String.make 5 ' ' in let r = get_buf db "Key4" s in (s,r) ));
	"6">:: (fun () -> assert_equal ("     ", -1) ( let s = String.make 5 ' ' in let r = get_buf db "UnusedKey" s in (s,r) ));
];
	
"set">::: [
	"1">:: (fun () -> assert_equal true (set_string db "Key10" "Value10_changed"));
	"2">:: (fun () -> assert_equal true (set db "Key15" (!!! "Value15")));
];

"replace">::: [
	"1">:: (fun () -> assert_equal true (replace db "Key13" (!!! "Value13|replaced")));
	"2">:: (fun () -> assert_equal true (replace_with_string db "Key0" "Value0|replaced"));
];

"remove">::: [
	"1">:: (fun () -> assert_equal true (remove db "Key13"));
	"2">:: (fun () -> assert_equal true (remove db "Key0"));
	"3">:: (fun () -> assert_equal false (remove db "UnknownKey"));
];

"seize">::: [
	"1">:: (fun () -> assert_equal_opt (!!! "Value10_changed") (seize db "Key10"));
	"2">:: (fun () -> assert_equal None (seize db "Key10"));
	"3">:: (fun () -> assert_equal None (seize db "UnusedKey"));
	"4">:: (fun () -> assert_equal true (add_string db "Key10" "Value10"));
	"5">:: (fun () -> assert_equal_opt "Value10" (seize_as_string db "Key10"));
	"6">:: (fun () -> assert_equal None (seize_as_string db "Key10"));
];


"iterate">::: [
	"1">:: (fun () -> let iter_func k v  o = o := Int64.add !o 1L; KCVISNOP in
		let icount = ref 0L in
		ignore(iterate db iter_func icount false); assert_equal (count db) !icount);
];

"incr">::: [
	"1">:: (fun () -> assert_equal 10L (incrint64 db "Keyincrint64" 10L 0L));
	"2">:: (fun () -> assert_equal 20L (incrint64 db "Keyincrint64" 10L 0L));
	"3">:: (fun () -> assert_equal 0L (incrint64 db "Keyincrint64" (-20L) 0L));
	"4">:: (fun () -> assert_equal 10. (incrfloat db "Keyincrfloat" 10. 0.));
	"5">:: (fun () -> assert_equal 20. (incrfloat db "Keyincrfloat" 10. 0.));
	"6">:: (fun () -> assert_equal  0. (incrfloat db "Keyincrfloat" (-20.) 0.));
];

"cas">::: [
	"1#">::. (fun () -> assert_equal true (add_string db "KeyCas" "ValueCas"));
	"2">::.. (fun () -> assert_equal false (cas db "KeyCas" (!!! "ValueCos") (!!! "ValueCas_"))); 
	"3">::. (fun () -> assert_equal true (cas db "KeyCas" (!!! "ValueCas") (!!! "ValueCas_"))); 
	"4">::. (fun () -> assert_equal true (cas_string db "KeyCas" "ValueCas_" (!!! "ValueCas__"))); 
	"4">::. (fun () -> assert_equal true (cas_stringstring db "KeyCas" "ValueCas__" "ValueCas___")); 
];

"transaction">::: [
	"1">:: (fun () -> assert_equal true (begintran db true));
	"2">:: (fun () -> assert_equal false (begintrantry db true));
	"3">:: (fun () -> assert_equal true (set_string db "Key7" "nil"));
	"4">:: (fun () -> assert_equal (Some "nil") (get_as_string db "Key7"));
	"5">:: (fun () -> assert_equal true (endtran db false));
	"6">:: (fun () -> assert_equal (Some "Value7") (get_as_string db "Key7"));

	"7">:: (fun () -> assert_equal true (begintran db true));
	"8">:: (fun () -> assert_equal false (begintrantry db true));
	"9">:: (fun () -> assert_equal true (set_string db "Key7" "nil"));
	"10">:: (fun () -> assert_equal (Some "nil") (get_as_string db "Key7"));
	"11">:: (fun () -> assert_equal true (endtran db true));
	"12">:: (fun () -> assert_equal (Some "nil") (get_as_string db "Key7"));
	"13">:: (fun () -> assert_equal true (set_string db "Key7" "Value7"));
];

"sync1">:: (fun () -> assert_bool "sync" ((sync db false Nullproc ())));

"snapshot">::: [
	"1">::(fun () -> assert_equal true (dumpsnap db ("test2" ^ name_of_suite ^ ".dump")));
	"1~">::(fun () -> assert_equal true (clear db));
	"2">::(fun () -> assert_equal true (loadsnap db ("test2" ^ name_of_suite ^ ".dump")));
	"2~">:: (fun () -> assert_equal_opt "Value5" (get_as_string db "Key5"));
];

"iterate2">::: [
	"1">:: (fun () -> let iter_func k v  o = 
			match (KCString.to_string k) with
			|"Key11" -> incr o; KCVISRESULT  (!!! "Value11iter")
			|"Key12" -> incr o; KCVISREMOVE
			| _ -> KCVISNOP in
		let o = ref 0 in
		assert_equal true (iterate db iter_func o false); 
		assert_equal !o 2
	);
	"2">:: (fun () -> assert_equal None (check db "Key12"));
	"3">:: (fun () -> assert_equal_opt "Value11iter" (get_as_string db "Key11"));
	"#3">:: (fun () -> assert_equal true (add_string db "Key12" "Value12"));
	"#3_">:: (fun () -> assert_equal true (set db "Key11" (!!!"Value11")));
	"4">:: (fun () -> let iter_func k v  o = 
			match k with
			|"Key12" -> incr o; KCVISRESULT  (!!! "Value12iter")
			|"Key11" -> incr o; KCVISREMOVE
			| _ -> KCVISNOP in
		let o = ref 0 in
		assert_equal true (iterate_string db iter_func o false);
		assert_equal !o 2
	);
	"5">:: (fun () -> assert_equal None (check db "Key11"));
	"6">:: (fun () -> assert_equal_opt (!!!"Value12iter") (get db "Key12"));
];

"match">::: [
	"1">:: (fun () -> assert_equal (1, "Keyincrint64") (let o = matchprefix db "Keyincrint" 1 
		in match o with Some a -> (Array.length a, Array.get a 0)| None -> -1, ""));
	"2">:: (fun () -> assert_equal 2 (let o = matchprefix db "Keyincr" 2 in match o with Some a -> Array.length a| None -> -1));
	"3">:: (fun () -> assert_equal 1 (let o = matchprefix db "Keyincrint" 10 in match o with Some a -> Array.length a| None -> -1));
	"4">:: (fun () -> assert_equal 1 (let o = matchprefix db "Keyincr" 1 in match o with Some a -> Array.length a| None -> -1));
	"5">:: (fun () -> assert_equal 4 (let o = matchregex db "Key1" 10 in match o with Some a -> Array.length a| None -> -1));
	"6">:: (fun () -> assert_equal 1 (let o = matchregex db "Key2" 5 in match o with Some a -> Array.length a| None -> -1));
	"7">:: (fun () -> assert_equal 1 (let o = matchsimilar db "Key5" 0 false 1 in match o with Some a -> Array.length a| None -> -1));
	"8">:: (fun () -> assert_equal 1 (let o = matchsimilar db "xey5" 1 false 5 in match o with Some a -> Array.length a| None -> -1));
];

"Xsub/Xtail">::: [
	"1">:: (fun () -> assert_equal true (addsub db "Key_addsub" (!!! "0123456789") 3 5));
	"2">:: (fun () -> assert_equal true (addtail db "Key_addtail" (!!! "0123456789") 6));
	"3">:: (fun () -> assert_equal_opt "34567" (get_as_string db "Key_addsub"));
	"4">:: (fun () -> assert_equal_opt "6789" (get_as_string db "Key_addtail"));
	"5">:: (fun () -> assert_equal true (setsub db "Key_addsub" (!!! "0123456789") 4 5));
	"6">:: (fun () -> assert_equal true (settail db "Key_addtail" (!!! "0123456789") 7));
	"7">:: (fun () -> assert_equal_opt "45678" (get_as_string db "Key_addsub"));
	"8">:: (fun () -> assert_equal_opt "789" (get_as_string db "Key_addtail"));
	"9">:: (fun () -> assert_equal true (replace_with_sub db "Key_addsub" (!!! "0123456789") 1 5));
	"10">:: (fun () -> assert_equal true (replace_with_tail db "Key_addtail" (!!! "0123456789") 8));
	"11">:: (fun () -> assert_equal_opt "12345" (get_as_string db "Key_addsub"));
	"12">:: (fun () -> assert_equal_opt "89" (get_as_string db "Key_addtail"));
	"13">:: (fun () -> assert_equal true (appendsub db "Key_addsub" (!!! "0123456789") 1 5));
	"14">:: (fun () -> assert_equal true (appendtail db "Key_addtail" (!!! "0123456789") 8));
	"15">:: (fun () -> assert_equal_opt "1234512345" (get_as_string db "Key_addsub"));
	"16">:: (fun () -> assert_equal_opt "8989" (get_as_string db "Key_addtail"));
];

"sync2">:: (fun () -> assert_bool "sync" ((sync db true Nullproc ())));
"size">:: (fun () -> assert_bool "No size available" ((size db) > 0L));
"path">:: (fun () -> assert_bool "No path available" ((path db) <> ""));
"close">:: (fun () -> assert_equal true (close_db db));
"GC">:: (fun () -> assert_equal () (Gc.full_major()));

]
;;


let _ = 
	print_endline "\n*** Delete old test data... ***";
	if 0 <> Sys.command "make cleandata" then begin
		print_endline "Unable to delete old test data.\nTest may deliver wrong results";
		print_string "Continue? [Yn] ";
		if (read_line ()) <> "Y" then exit 0
	else
		print_newline ();
	end;
	
	print_endline ">>> Test hash and tree databases <<<";
	print_endline ">>> Modules: Kyotocaml.KCDb, Kyotocaml.KCDb.Ret,  Kyotocaml.KCCur.Ret <<<";
	let opendb = open_proto_hash_db ~log:`Stderr ~logkinds:`Error ~logpx:`None in
	let t1 = (get_test_suite opendb "Proto Hash" openopt_rwc true) in

	let opendb = open_proto_tree_db ~log:`Stderr ~logkinds:`Error ~logpx:`None in
	let t2 = (get_test_suite opendb "Proto Tree" openopt_rwc false) in

	let opendb = 
		fun db -> open_stash_db ~log:`Stderr ~logkinds:`Error ~logpx:`None ~bnum:`None db in
	let t3 = (get_test_suite opendb "Stash" openopt_rwc true) in

	let opendb = 
		fun db -> open_cache_hash_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db 
		~opts:`None ~bnum:`None ~zcmp:`None ~capcnt:`None ~capsiz:`None ~zkey:`None in
	let t4 = (get_test_suite opendb "Cache Hash" openopt_rwc true) in

	let opendb = 
		fun db -> open_cache_tree_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db 
		~opts:`None ~bnum:`None ~zcmp:`None ~capcnt:`None ~capsiz:`None ~zkey:`None 
		~psiz:`None ~rcomp:`None ~pccap:`None in
	let t5 = (get_test_suite opendb "Cache Tree" openopt_rwc false)  in

	let opendb = 
		fun db -> open_file_hash_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db "." "file_hash_1" in
	let t6 = (get_test_suite opendb "File Hash" openopt_rwc true) in

	let opendb = 
		fun db -> open_file_tree_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db "." "file_tree_1" in
	let t7 = (get_test_suite opendb "File Tree" openopt_rwc false) in

	let opendb = 
		fun db -> open_dir_hash_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db "." "dir_hash_1" in
	let t8 = (get_test_suite opendb "Dir Hash" openopt_rwc true) in

	let opendb = 
		fun db -> open_dir_tree_db ~log:`Stderr ~logkinds:`Error ~logpx:`None db "." "dir_tree_1" in
	let t9 = (get_test_suite opendb "Dir Tree" openopt_rwc false) in
	
	let test = "kyotocaml[RET]" >:::[t1; t2; t3; t4; t5; t6; t7; t8; t9 ] in
	run_test_tt_main test
;;

