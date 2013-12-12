open Unix;;
open OUnit;;
open Kyotocaml;;
open KCDb;;
open Exc;;

module Cur = KCCur.Exc
;;

let ( !!! ) = KCString.of_string
;;

let cur:(KCCur.t option ref) = ref None
;;

let cur_else_skip () = match !cur with | Some c -> c | None -> skip_if true "No Cursor created"; raise (Failure "")
;;

let keynr a = Scanf.sscanf (KCString.to_string a) "Key%d" (fun n -> n)
;;

let valuenr a = Scanf.sscanf (KCString.to_string a) "Value%d" (fun n -> n)
;;

let assert_unit = assert_equal ()
;;

let get_test_suite opendb name_of_suite is_hash = 
let db = make () in 
let ( >::. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal ec (ecode db) in
let ( >::.. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal true (ec <> (ecode db)) in
let skip_if_hash () = skip_if is_hash "Not implemented" in
name_of_suite >:::[
	"new db">:: (fun () -> assert_unit (Printf.printf "\n%!"));
	"open">:: (fun () -> 
		let openopts = [KCOWRITER; KCOCREATE; KCOTRUNCATE] in
		assert_unit (opendb db openopts));
	"ecode">:: (fun () -> assert_equal 0 (ecode db));
	"version">::(fun () -> let ver = version () in assert_bool ver ("" <> ver));

"add">::: [
	"0">::. (fun () -> assert_unit  (add_string db "Key0" "Value0"));
	"1">::. (fun () -> assert_unit (add_string db "Key1" "Value1"));
	"2">::. (fun () -> assert_unit (add_string db "Key2" "Value2"));
	"3">::. (fun () -> assert_unit (add_string db "Key3" "Value3"));
	"4">::. (fun () -> assert_unit (add db "Key4" (!!! "Value4")));
	"5">::. (fun () -> assert_unit (add_string db "Key5" "Value5"));
	"6">::. (fun () -> assert_unit (add_string db "Key6" "Value6"));
	"7">::. (fun () -> assert_unit (add db "Key7" (!!! "Value7")));
	"8">::. (fun () -> assert_unit (add_string db "Key8" "Value8"));
	"9">::. (fun () -> assert_unit (add_string db "Key9" "Value9"));
	"10">::. (fun () -> assert_unit (add_string db "Key10" "Value10"));
	"10a">::.. (fun () -> assert_raises (Failure "kcdb.add_string") (fun () -> add_string db "Key10" "Value10"));
	"11">::. (fun () -> assert_unit (add_string db "Key11" "Value11"));
	"12">::. (fun () -> assert_unit (add_string db "Key12" "Value12"));
	"13">::. (fun () -> assert_unit (add db "Key13" (!!! "Value13")));
	"14">::. (fun () -> assert_unit (add db "Key14" (!!! "Value14")));
	"15">::. (fun () -> assert_unit (add db "Key15" (!!! "Value15")));
];
	
"check">::: [
	"1">::. (fun () -> assert_equal 6 (check db "Key1"));
	"2">:: (fun () -> assert_raises (Key_not_found("kcdb.check")) (fun () -> check db "xxx"));
	"3">:: (fun () -> assert_equal 7 (check db "Key10"));
];

"cursor">::: [
	"1">:: (fun () -> assert_equal  () (cur := Some(cursor db)));

	"2">:: 
	(fun () -> assert_equal  () (Cur.jump (cur_else_skip())) );

	"3">:: 
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  true (keynr(Cur.getkey c false) = valuenr(Cur.getvalue c false))
	);
		
	"4">:: 
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  true (let n = keynr(Cur.getkey c false) in n = valuenr(Cur.getvalue c true)) 
	);
		
	"5">:: 
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal  ("Key", "Value") (let (k, v) = Cur.get_as_string c false in (String.sub k 0 3, String.sub v 0 5))
	);
		
	"6">::
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal true (let (k,v) = Cur.get c false in keynr k = valuenr v)
	);
		
	"7">:: 
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal true (let (k,v) = Cur.get c true in keynr k = valuenr v)
	);
		
	"8">::
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal true (let (k,v) = (Cur.get c true) in keynr k = valuenr v)
	);
		
	"9">::(fun () -> assert_equal  () (Cur.jumpkey (cur_else_skip()) "Key2" ));
	"10">:: (fun () -> assert_equal  () (Cur.step (cur_else_skip())));
	"11">::(fun () -> skip_if_hash (); assert_equal  () (Cur.jumpback (cur_else_skip())));
	
	"12">:: 
	(fun () -> 
		let c = cur_else_skip() in
		assert_equal true (let (k,v) = Cur.get c true in (keynr k = valuenr v))
	);

	"13">:: 
	(fun () -> 
		skip_if_hash (); 
		let c = cur_else_skip() in
		assert_equal  () (Cur.stepback c);
	);

	"14">:: 
	(fun () ->  
		let c = cur_else_skip() in
		Cur.jump c;
		let v = !!! "Cur.setvalue#" in
		assert_unit (Cur.setvalue c v true)
	);

	"15">::
	(fun () ->  
		let c = cur_else_skip() in
		let v = "Cur.setvalue##" in
		assert_unit (Cur.setvalue_string c v false)
	);

	"16">::
	(fun () ->  
		let c = cur_else_skip() in
		let v = "Cur.setvalue##" in
		let v' = Cur.getvalue_as_string c false in
		assert_equal v v'
	);

	"17">:: 
	(fun () -> 
		let c = cur_else_skip() in
		Cur.jump c;
		let v = !!! "Cur.setvalue#" in
		assert_equal v (Cur.getvalue c false)
	);
];

"append">::: [
	"1">:: (fun () -> assert_unit (append_string db "Key1" "+append_string"));
	"2">:: (fun () -> assert_unit (append db "Key2" (!!! "+append")));
	"3">:: (fun () -> assert_raises (Failure("kcdb.add_string")) (fun () -> add_string db "Key2" "+add_string"));
	"4">:: (fun () -> assert_unit (append_string db "Key11" "+append_string"));
];
	
"get">::: [
	"1">:: (fun () -> assert_equal "Value5" (get_as_string db "Key5"));
	"2">:: (fun () -> assert_equal (!!! "Value5") (get db "Key5"));
	"3">:: (fun () -> assert_equal (!!! "Value11+append_string") (get db "Key11"));
	"4">:: (fun () -> assert_raises (Key_not_found("kcdb.get_as_string")) (fun () -> get_as_string db "UnusedKey"));
	"5">:: (fun () -> assert_equal ("Value", 6) ( let s = String.make 5 ' ' in let r = get_buf db "Key4" s in (s,r) ));
	"6">:: (fun () -> assert_raises (Key_not_found("kcdb.get_buf")) (fun() -> let s = String.make 5 ' ' in let r = get_buf db "UnusedKey" s in (s,r) ));
];
	
"set">::: [
	"1">:: (fun () -> assert_unit (set_string db "Key10" "Value10_changed"));
	"2">:: (fun () -> assert_unit (set db "Key15" (!!! "Value15")));
];

"replace">::: [
	"1">:: (fun () -> assert_unit (replace db "Key13" (!!! "Value13|replaced")));
	"2">:: (fun () -> assert_unit (replace_with_string db "Key0" "Value0|replaced"));
];

"remove">::: [
	"1">:: (fun () -> assert_unit (remove db "Key13"));
	"2">:: (fun () -> assert_unit (remove db "Key0"));
	"3">:: (fun () -> assert_raises (Key_not_found("kcdb.remove")) (fun () -> remove db "UnknownKey"));
];

"seize">::: [
	"1">:: (fun () -> assert_equal (!!! "Value10_changed") (seize db "Key10"));
	"2">:: (fun () -> assert_raises (Key_not_found("kcdb.seize")) (fun () -> seize db "Key10"));
	"3">:: (fun () -> assert_raises (Key_not_found("kcdb.seize")) (fun () -> seize db "UnusedKey"));
	"4">:: (fun () -> assert_unit (add_string db "Key10" "Value10"));
	"5">:: (fun () -> assert_equal "Value10" (seize_as_string db "Key10"));
	"6">:: (fun () -> assert_raises (Key_not_found("kcdb.seize_as_string")) (fun() -> seize_as_string db "Key10"));
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
	"1#">::. (fun () -> assert_equal () (add_string db "KeyCas" "ValueCas"));
	"2">::.. (fun () -> assert_equal false (cas db "KeyCas" (!!! "ValueCos") (!!! "ValueCas_"))); 
	"3">::. (fun () -> assert_equal true (cas db "KeyCas" (!!! "ValueCas") (!!! "ValueCas_"))); 
	"4">::. (fun () -> assert_equal true (cas_string db "KeyCas" "ValueCas_" (!!! "ValueCas__"))); 
	"4">::. (fun () -> assert_equal true (cas_stringstring db "KeyCas" "ValueCas__" "ValueCas___")); 
];

"transaction">::: [
	"1">:: (fun () -> assert_unit (begintran db true));
	"2">:: (fun () -> assert_raises (Failure "kcdb.begintrantry") (fun () -> begintrantry db true));
	"3">:: (fun () -> assert_unit (set_string db "Key7" "nil"));
	"4">:: (fun () -> assert_equal "nil" (get_as_string db "Key7"));
	"5">:: (fun () -> assert_unit (endtran db false));
	"6">:: (fun () -> assert_equal "Value7" (get_as_string db "Key7"));

	"7">:: (fun () -> assert_equal  () (begintran db true));
	"8">:: (fun () -> assert_raises (Failure "kcdb.begintrantry") (fun () -> begintrantry db true));
	"9">:: (fun () -> assert_unit (set_string db "Key7" "nil"));
	"10">:: (fun () -> assert_equal "nil" (get_as_string db "Key7"));
	"11">:: (fun () -> assert_unit (endtran db true));
	"12">:: (fun () -> assert_equal "nil" (get_as_string db "Key7"));
	"13">:: (fun () -> assert_unit (set_string db "Key7" "Value7"));
];

"sync1">:: (fun () -> assert_unit ((sync db false Nullproc ())));


"snapshot">::: [
	"1">::(fun () -> assert_equal true (dumpsnap db ("test2" ^ name_of_suite ^ ".dump")));
	"1~">::(fun () -> assert_unit (clear db));
	"2">::(fun () -> assert_equal true (loadsnap db ("test2" ^ name_of_suite ^ ".dump")));
	"2~">:: (fun () -> assert_equal "Value5" (get_as_string db "Key5"));
];

"iterate2">::: [
	"1">:: (fun () -> let iter_func k v  o = 
			match (KCString.to_string k) with
			|"Key11" -> incr o; KCVISRESULT  (!!! "Value11iter")
			|"Key12" -> incr o; KCVISREMOVE
			| _ -> KCVISNOP in
		let o = ref 0 in
		assert_equal () (iterate db iter_func o false); 
		assert_equal !o 2
	);
	"2">:: (fun () -> assert_raises (Key_not_found("kcdb.check")) (fun() -> check db "Key12"));
	"3">:: (fun () -> assert_equal "Value11iter" (get_as_string db "Key11"));
	"#3">:: (fun () -> assert_equal () (add_string db "Key12" "Value12"));
	"#3_">:: (fun () -> assert_equal () (set db "Key11" (!!!"Value11")));
	"4">:: (fun () -> let iter_func k v  o = 
			match k with
			|"Key12" -> incr o; KCVISRESULT  (!!! "Value12iter")
			|"Key11" -> incr o; KCVISREMOVE
			| _ -> KCVISNOP in
		let o = ref 0 in
		assert_equal () (iterate_string db iter_func o false);
		assert_equal !o 2
	);
	"5">:: (fun () -> assert_raises (Key_not_found("kcdb.check")) (fun() -> check db "Key11"));
	"6">:: (fun () -> assert_equal (!!!"Value12iter") (get db "Key12"));
];

"match">::: [
	"1">:: (fun () -> assert_equal (1, "Keyincrint64") 
		(let a = matchprefix db "Keyincrint" 1 in (Array.length a, Array.get a 0)));
	"2">:: (fun () -> assert_equal 2 (let a = matchprefix db "Keyincr" 2 in Array.length a));
	"3">:: (fun () -> assert_equal 1 (let a = matchprefix db "Keyincrint" 10 in Array.length a));
	"4">:: (fun () -> assert_equal 1 (let a = matchprefix db "Keyincr" 1 in Array.length a));
	"5">:: (fun () -> assert_equal 4 (let a = matchregex db "Key1" 10 in Array.length a));
	"6">:: (fun () -> assert_equal 1 (let a = matchregex db "Key2" 5 in Array.length a));
	"7">:: (fun () -> assert_equal 1 (let a = matchsimilar db "Key5" 0 false 1 in Array.length a));
	"8">:: (fun () -> assert_equal 1 (let a = matchsimilar db "xey5" 1 false 5 in Array.length a));
];

"Xsub/Xtail">::: [
	"1">:: (fun () -> assert_equal () (addsub db "Key_addsub" (!!! "0123456789") 3 5));
	"2">:: (fun () -> assert_equal () (addtail db "Key_addtail" (!!! "0123456789") 6));
	"3">:: (fun () -> assert_equal "34567" (get_as_string db "Key_addsub"));
	"4">:: (fun () -> assert_equal "6789" (get_as_string db "Key_addtail"));
	"5">:: (fun () -> assert_equal () (setsub db "Key_addsub" (!!! "0123456789") 4 5));
	"6">:: (fun () -> assert_equal () (settail db "Key_addtail" (!!! "0123456789") 7));
	"7">:: (fun () -> assert_equal "45678" (get_as_string db "Key_addsub"));
	"8">:: (fun () -> assert_equal "789" (get_as_string db "Key_addtail"));
	"9">:: (fun () -> assert_equal () (replace_with_sub db "Key_addsub" (!!! "0123456789") 1 5));
	"10">:: (fun () -> assert_equal () (replace_with_tail db "Key_addtail" (!!! "0123456789") 8));
	"11">:: (fun () -> assert_equal "12345" (get_as_string db "Key_addsub"));
	"12">:: (fun () -> assert_equal "89" (get_as_string db "Key_addtail"));
	"13">:: (fun () -> assert_equal () (appendsub db "Key_addsub" (!!! "0123456789") 1 5));
	"14">:: (fun () -> assert_equal () (appendtail db "Key_addtail" (!!! "0123456789") 8));
	"15">:: (fun () -> assert_equal "1234512345" (get_as_string db "Key_addsub"));
	"16">:: (fun () -> assert_equal "8989" (get_as_string db "Key_addtail"));
];

"sync2">:: (fun () -> assert_unit ((sync db true Nullproc ())));
"size">:: (fun () -> assert_bool "No size available" ((size db) > 0L));
"path">:: (fun () -> assert_bool "No path available" ((path db) <> ""));
"close">:: (fun () -> assert_unit (close_db db));
"GC">:: (fun () -> assert_unit (Gc.full_major()));

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
	print_endline ">>> Modules: Kyotocaml.KCDb.Exc,  Kyotocaml.KCCur.Esc <<<";
	let opendb = open_proto_hash_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None in
	let t1 = (get_test_suite opendb "Proto Hash" true) in

	let opendb = open_proto_tree_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None in
	let t2 = (get_test_suite opendb "Proto Tree" false) in

	let opendb = 
		fun db -> open_stash_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None ~bnum:`None db in
	let t3 = (get_test_suite opendb "Stash" true) in

	let opendb = 
		fun db -> open_cache_hash_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None db 
		~opts:`None ~bnum:`None ~zcmp:`None ~capcnt:`None ~capsiz:`None ~zkey:`None in
	let t4 = (get_test_suite opendb "Cache Hash" true) in

	let opendb = 
		fun db -> open_cache_tree_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None db 
		~opts:`None ~bnum:`None ~zcmp:`None ~capcnt:`None ~capsiz:`None ~zkey:`None 
		~psiz:`None ~rcomp:`None ~pccap:`None in
	let t5 = (get_test_suite opendb "Cache Tree"  false)  in

	let opendb = 
		fun db -> open_file_hash_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None db "." "file_hash_1" in
	let t6 = (get_test_suite opendb "File Hash" true) in

	let opendb = 
		fun db -> open_file_tree_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None db "." "file_tree_1" in
	let t7 = (get_test_suite opendb "File Tree" false) in

	let opendb = 
		fun db -> open_dir_hash_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None db "." "dir_hash_1" in
	let t8 = (get_test_suite opendb "Dir Hash" true) in

	let opendb = 
		fun db -> open_dir_tree_db ~log:(`Path "2x.log") ~logkinds:`Error ~logpx:`None db "." "dir_tree_1" in
	let t9 = (get_test_suite opendb "Dir Tree" false) in
	
	let test = "kyotocaml[EXC]" >:::[t1; t2; t3; t4; t5; t6; t7; t8; t9 ] in
	run_test_tt_main test
;;

