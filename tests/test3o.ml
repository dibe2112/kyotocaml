open Unix;;
open OUnit;;
open Kyotocaml;;
open KCIdx;;
open Ret;;

let assert_equal_opt a = assert_equal (Some a)
;;

let openopt_rwc = [KCDb.KCOREADER; KCDb.KCOWRITER; KCDb.KCOCREATE]
;;

let dopt (Some o) = o
;;

let get_test_suite opendb name_of_suite openopt is_hash = 
let db = match make () with 
	| Some db -> db 
	| None -> prerr_endline "Fatal error: Cannot create database objekt\nTest is terminated"; exit (-1) in 
let ( >::. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal ec (ecode db) in
let ( >::.. ) lbl f = lbl >:: fun () -> let ec = ecode db in f(); assert_equal true (ec <> (ecode db)) in
name_of_suite >:::[
	"new db">:: (fun () -> assert_equal () (Printf.printf "\n%!"));
	"open">:: (fun () -> assert_equal true (opendb db openopt));
	"ecode">:: (fun () -> assert_equal 0 (ecode db));
	"version">::(fun () -> let ver = version () in assert_bool ver ("" <> ver));

"add">::: [
	"0">:: (fun () -> assert_equal true  (add_string db "Key0" "Value0"));
	"1">::. (fun () -> assert_equal true (add_string db "Key1" "Value1"));
	"2">::. (fun () -> assert_equal true (add_string db "Key2" "Value2"));
	"3">::. (fun () -> assert_equal true (add_string db "Key3" "Value3"));
	"4">::. (fun () -> assert_equal true (add db "Key4" (KCString.of_string "Value4")));
	"5">::. (fun () -> assert_equal true (add_string db "Key5" "Value5"));
	"6">::. (fun () -> assert_equal true (add_string db "Key6" "Value6"));
	"7">::. (fun () -> assert_equal true (add db "Key7" (KCString.of_string "Value7")));
	"8">::. (fun () -> assert_equal true (add_string db "Key8" "Value8"));
	"9">::. (fun () -> assert_equal true (add_string db "Key9" "Value9"));
	"10">::. (fun () -> assert_equal true (add_string db "Key10" "Value10"));
	"10a">::.. (fun () -> assert_equal false (add_string db "Key10" "Value10"));
	"11">:: (fun () -> assert_equal true (add_string db "Key11" "Value11"));
	"12">::. (fun () -> assert_equal true (add_string db "Key12" "Value12"));
];
	
"append">::: [
	"1">:: (fun () -> assert_equal true (append_string db "Key1" "+append_string"));
	"2">:: (fun () -> assert_equal true (append db "Key2" (KCString.of_string "+append")));
	"3">:: (fun () -> assert_equal false (add_string db "Key2" "+add_string"));
	"4">:: (fun () -> assert_equal true (append_string db "Key11" "+append_string"));
];
	
"get">::: [
	"1">:: (fun () -> assert_equal_opt "Value5" (get_as_string db "Key5"));
	"2">:: (fun () -> assert_equal_opt (KCString.of_string "Value5") (get db "Key5"));
	"3">:: (fun () -> assert_equal_opt (KCString.of_string "Value11+append_string") (get db "Key11"));
	"4">:: (fun () -> assert_equal None (get_as_string db "UnusedKey"));
];
	
"set">::: [
	"1">:: (fun () -> assert_equal true (set_string db "Key10" "Value10_changed"));
	"2">:: (fun () -> assert_equal true (set db "Key15" (KCString.of_string "Value15")));
];

"Xsub/Xtail">::: [
	"1">:: (fun () -> assert_equal true (addsub db "Key_addsub" (KCString.of_string "0123456789") 3 5));
	"2">:: (fun () -> assert_equal true (addtail db "Key_addtail" (KCString.of_string "0123456789") 6));
	"3">:: (fun () -> assert_equal_opt "34567" (get_as_string db "Key_addsub"));
	"4">:: (fun () -> assert_equal_opt "6789" (get_as_string db "Key_addtail"));
	"5">:: (fun () -> assert_equal true (setsub db "Key_addsub" (KCString.of_string "0123456789") 4 5));
	"6">:: (fun () -> assert_equal true (settail db "Key_addtail" (KCString.of_string "0123456789") 7));
	"7">:: (fun () -> assert_equal_opt "45678" (get_as_string db "Key_addsub"));
	"8">:: (fun () -> assert_equal_opt "789" (get_as_string db "Key_addtail"));
	"9">:: (fun () -> assert_equal true (replace_with_sub db "Key_addsub" (KCString.of_string "0123456789") 1 5));
	"10">:: (fun () -> assert_equal true (replace_with_tail db "Key_addtail" (KCString.of_string "0123456789") 8));
	"11">:: (fun () -> assert_equal_opt "12345" (get_as_string db "Key_addsub"));
	"12">:: (fun () -> assert_equal_opt "89" (get_as_string db "Key_addtail"));
	"13">:: (fun () -> assert_equal true (appendsub db "Key_addsub" (KCString.of_string "0123456789") 1 5));
	"14">:: (fun () -> assert_equal true (appendtail db "Key_addtail" (KCString.of_string "0123456789") 8));
	"15">:: (fun () -> assert_equal_opt "1234512345" (get_as_string db "Key_addsub"));
	"16">:: (fun () -> assert_equal_opt "8989" (get_as_string db "Key_addtail"));
];

"sync">:: (fun () -> assert_bool "sync" ((sync db true Nullproc ())));
"size">:: (fun () -> assert_bool "No size available" ((size db) > 0L));
"close">:: (fun () -> assert_equal true (close_db db));
"GC">:: (fun () -> assert_equal () (Gc.full_major()));

]
;;


let _ = 
	let logpth = `Path "3o.log" in
	print_endline "\n*** Delete old test data... ***";
	if 0 <> Sys.command "make cleandata" then begin
		print_endline "Unable to delete old test data.\nTest may deliver wrong results";
		print_string "Continue? [Yn] ";
		if (read_line ()) <> "Y" then exit 0
	else
		print_newline ();
	end;
	
	print_endline ">>> Test index database <<<";
	print_endline ">>> Modules: Kyotocaml.KCIdx, Kyotocaml.KCIdx.Ret <<<";
	let opendb = open_proto_hash_db ~log:logpth ~logkinds:`Error ~logpx:`None in
	let t1 = (get_test_suite opendb "Proto Hash" openopt_rwc true) in

	let opendb = open_proto_tree_db ~log:`Stderr ~logkinds:`Error ~logpx:`None in
	let t2 = (get_test_suite opendb "Proto Tree" openopt_rwc false) in

	let opendb = 
		fun db -> open_stash_db ~log:logpth ~logkinds:`Error ~logpx:`None ~bnum:`None db in
	let t3 = (get_test_suite opendb "Stash" openopt_rwc true) in

	let opendb = 
		fun db -> open_cache_hash_db ~log:logpth ~logkinds:`Error ~logpx:`None db 
		~opts:`None ~bnum:`None ~zcmp:`None ~capcnt:`None ~capsiz:`None ~zkey:`None in
	let t4 = (get_test_suite opendb "Cache Hash" openopt_rwc true) in

	let opendb = 
		fun db -> open_cache_tree_db ~log:logpth ~logkinds:`Error ~logpx:`None db 
		~opts:`None ~bnum:`None ~zcmp:`None ~capcnt:`None ~capsiz:`None ~zkey:`None 
		~psiz:`None ~rcomp:`None ~pccap:`None in
	let t5 = (get_test_suite opendb "Cache Tree" openopt_rwc false)  in

	let opendb = 
		fun db -> open_file_hash_db ~log:logpth ~logkinds:`Error ~logpx:`None db "." "file_hash_1" in
	let t6 = (get_test_suite opendb "File Hash" openopt_rwc true) in

	let opendb = 
		fun db -> open_file_tree_db ~log:logpth ~logkinds:`Error ~logpx:`None db "." "file_tree_1" in
	let t7 = (get_test_suite opendb "File Tree" openopt_rwc false) in

	let opendb = 
		fun db -> open_dir_hash_db ~log:logpth ~logkinds:`Error ~logpx:`None db "." "dir_hash_1" in
	let t8 = (get_test_suite opendb "Dir Hash" openopt_rwc true) in

	let opendb = 
		fun db -> open_dir_tree_db ~log:logpth ~logkinds:`Error ~logpx:`None db "." "dir_tree_1" in
	let t9 = (get_test_suite opendb "Dir Tree" openopt_rwc false) in
	
	let test = "kyotocaml" >:::[t1; t2; t3; t4; t5; t6; t7; t8; t9 ] in
	run_test_tt_main test
;;

