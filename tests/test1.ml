open Unix;;
open OUnit;;
open Kyotocaml;;

module M = Kyotocaml.KCString;;

let digits = "0123456789";;

let kcs_digits = M.of_string digits;;
let kcs_5a = M.make 5 'a';;
let kcs_empty = M.create 0;;
let kcs_empty2 = M.of_string "";;

let kcs_from_int = M.of_int 1234;;
let kcs_from_int32 = M.of_int32 1234l;;
let kcs_from_int64 = M.of_int64 1234L;;
let kcs_from_intnat = M.of_intnat 1234n;;
let kcs_from_float = M.of_float 12.343 2;;
let kcs_1234 = M.of_string "1234";;
let kcs_12_34 = M.of_string "12.34";;

let kcs_0 = M.of_int 0;;

let ( !!! ) = M.of_string
;;

let s = ref "";;

let kcstringT = 
"kcstring">:::
[
	"make">::: [
		"1">:: (fun () -> assert_equal (M.of_string "aaaaa") kcs_5a);
		"2">:: (fun () -> assert_equal kcs_empty kcs_empty2);
	];

	"custom ops">::: [
		"cmp1">:: (fun () -> assert_equal 0 (compare  kcs_from_int32  kcs_from_int64));
		"cmp2">:: (fun () -> assert_equal (-1) (compare  kcs_0  kcs_from_int64));
		"cmp3">:: (fun () -> assert_equal 1 (compare  kcs_from_int64 kcs_0));

		"marshal1">:: (fun () -> 
				s := Marshal.to_string kcs_digits [];
				Printf.printf "%S%!" !s;
				assert_equal true (!s <> "") ); 
				
		"marshal2">:: (fun () -> 
				let kcs = Marshal.from_string !s 0 in
				assert_equal kcs kcs_digits);
				
		"hash1">:: (fun () -> 
			assert_bool "Hashtbl.hash may fail" ((Hashtbl.hash kcs_digits) != (Hashtbl.hash kcs_from_int)));

		"hash2">:: (fun () -> 
			assert_bool "Hashtbl.hash may fail" ((Hashtbl.hash kcs_from_int64) = (Hashtbl.hash kcs_from_int)));
	];
	
	"from_">::: [
		"int">:: (fun () -> assert_equal kcs_1234 kcs_from_int);
		"int32">:: (fun () -> assert_equal kcs_1234 kcs_from_int32);
		"int64">:: (fun () -> assert_equal kcs_1234 kcs_from_int64);
		"intnat">:: (fun () -> assert_equal kcs_1234 kcs_from_intnat);
		"float">:: (fun () -> assert_equal kcs_12_34 kcs_from_float);
	];
	
	"length">::: [
		"1">:: (fun () -> assert_equal 10 (M.length kcs_digits));
		"2">:: (fun () -> assert_equal 5 (M.length kcs_5a));
		"3">:: (fun () -> assert_equal 0 (M.length kcs_empty));
		"4">:: (fun () -> assert_equal 0 (M.length kcs_empty2));
	];
	
	"get">::: [
		"1">:: (fun () -> assert_equal '0' (M.get kcs_digits 0));
		"2">:: (fun () -> assert_equal '0' (M.unsafe_get kcs_digits 0));
		"3">:: (fun () -> assert_equal '9' (M.get kcs_digits 9));
		"4">:: (fun () -> assert_equal '9' (M.unsafe_get kcs_digits 9));
		"5">:: (fun () -> assert_equal '2' (M.get kcs_digits 2));
		"6">:: (fun () -> assert_equal '2' (M.unsafe_get kcs_digits 2));
		"7">:: (fun () -> assert_equal '4' (M.get kcs_digits 4));
		"8">:: (fun () -> assert_equal '4' (M.unsafe_get kcs_digits 4));
		"9">:: (fun () -> assert_raises (Invalid_argument "kcstring.get") (fun () -> M.get kcs_empty 0));
		"10">:: (fun () -> assert_raises (Invalid_argument "kcstring.get") (fun () -> M.get kcs_empty2 0));
		"11">:: (fun () -> assert_raises (Invalid_argument "kcstring.get") (fun () -> M.get kcs_digits (-1)));
		"12">:: (fun () -> assert_raises (Invalid_argument "kcstring.get") (fun () -> M.get kcs_digits 10));
	];
	
	"set">::: [
		"1">:: (fun () -> assert_equal () (M.set kcs_digits 0 'A'));
		"2">:: (fun () -> assert_equal 'A' (M.get kcs_digits 0));
		"3">:: (fun () -> assert_equal () (M.set kcs_digits 0 '0'));
		"4">:: (fun () -> assert_equal '0' (M.get kcs_digits 0));
		"5">:: (fun () -> assert_equal () (M.set kcs_digits 9 'Z'));
		"6">:: (fun () -> assert_equal 'Z' (M.get kcs_digits 9));
		"7">:: (fun () -> assert_equal () (M.set kcs_digits 9 '9'));
		"8">:: (fun () -> assert_equal '9' (M.get kcs_digits 9));
		"9">:: (fun () -> assert_raises (Invalid_argument "kcstring.set") (fun () -> M.set kcs_empty 0 '*'));
		"10">:: (fun () -> assert_raises (Invalid_argument "kcstring.set") (fun () -> M.set kcs_empty2 0 '*'));
		"11">:: (fun () -> assert_raises (Invalid_argument "kcstring.set") (fun () -> M.set kcs_digits (-1) '*'));
		"12">:: (fun () -> assert_raises (Invalid_argument "kcstring.set") (fun () -> M.set kcs_digits 10 '*'));
	];
	
	"compare">::: [
		"1">:: (fun () -> assert_equal 0 (M.compare (!!! "abcd") (!!! "abcd")));
		"2">:: (fun () -> assert_equal (-1) (M.compare (!!! "xabcd") (!!! "yabcd")));
		"3">:: (fun () -> assert_equal (-1) (M.compare_nocase (!!! "abcd") (!!! "abcde")));
		"4">:: (fun () -> assert_equal (1) (M.compare_nocase (!!! "abcdef") (!!! "abcde")));
		"5">:: (fun () -> assert_equal (-1) (M.compare_nocase (!!! "abcd") (!!! "ABcde")));
		"6">:: (fun () -> assert_equal (-1) (M.compare_nocase (!!! "abcd") (!!! "bcde")));
		"7">:: (fun () -> assert_equal (1) (M.compare_nocase (!!! "ybcde") (!!! "xbcde")));
		"8">:: (fun () -> assert_equal 0 (M.compare_nocase (!!! "abcd") (!!! "ABcd")));
		"9">:: (fun () -> assert_equal 0 (M.compare_nocase_with_string (!!! "abcd") "ABcd"));
	];
	
	"sub">::: [
		"1">:: (fun () -> assert_equal (M.of_string "34567") (M.sub kcs_digits 3 5));
		"2">:: (fun () -> assert_equal kcs_digits (M.sub kcs_digits 0 (M.length kcs_digits)));
		"3">:: (fun () -> assert_raises (Invalid_argument "kcstring.sub") (fun () -> M.sub kcs_digits 5 6));
		"4">:: (fun () -> assert_raises (Invalid_argument "kcstring.sub") (fun () -> M.sub kcs_digits (-5) 6));
		"5">:: (fun () -> assert_equal "34567" (M.sub_as_string kcs_digits 3 5));
		"6">:: (fun () -> assert_equal digits (M.sub_as_string kcs_digits 0 (M.length kcs_digits)));
		"7">:: (fun () -> assert_raises (Invalid_argument "kcstring.sub_as_string") (fun () -> M.sub_as_string kcs_digits 5 6));
		"8">:: (fun () -> assert_raises (Invalid_argument "kcstring.sub_as_string") (fun () -> M.sub_as_string kcs_digits (-5) 6));
	];

	"concat">::: [
		
		"0">:: (fun () -> assert_equal (!!!"123__456__789") (M.concat [!!!"123"; !!!"456"; !!!"789"] (!!!"__")));
		"1">::(fun () -> assert_equal (!!!"123__456__789") (M.concats [!!!"123"; !!!"456"; !!!"789"] "__"));
		"2">:: (fun () -> assert_equal "123__456__789" (M.concat_to_string [!!!"123"; !!!"456"; !!!"789"] (!!!"__")));
		"3">:: (fun () -> assert_equal "123" (M.concat_to_string [!!!"123"] (!!!"__")));
		"4">:: (fun () -> assert_equal "" (M.concat_to_string [] (!!!"__")));
	];
	
	"find" >::: [
		"1">:: (fun () -> assert_equal 4 (M.find (!!!"0123456789") (!!!"45")));
		"2">:: (fun () -> assert_equal 4 (M.find_string (!!!"0123456789") "45"));
		"3">:: (fun () -> assert_equal 4 (M.find_string ~offset:4 (!!!"0123456789") "45"));
		"4">:: (fun () -> assert_raises (Invalid_argument "kcstring.find") (fun() -> M.find ~offset:11 (!!!"0123456789") (!!!"54")));
		"5">:: (fun () -> assert_raises Not_found (fun() -> M.find_string (!!!"0123456789") "54"));
		"6">:: (fun () -> assert_raises Not_found (fun() -> M.find_string (!!!"0123456789") "7890"));
		"7">:: (fun () -> assert_raises Not_found (fun() -> M.find_string ~offset:6 (!!!"0123456789") "54"));
		"8">:: (fun () -> assert_raises (Invalid_argument "kcstring.find") (fun() -> M.find_string ~offset:11 (!!!"0123456789") "54"));
	];
	
	"ato?">:::[
		"1">:: (fun () -> assert_equal 12345L (M.atoi 0 (!!!"12345")));
		"2">:: (fun () -> assert_equal 2345L (M.atoi 1 (!!!"12345")));
		"3">:: (fun () -> assert_equal 345L (M.atoi 2 (!!!"12345")));
		"4">:: (fun () -> assert_raises (Invalid_argument "kcstring.atoi") (fun() -> (M.atoi 5 (!!!"12345"))));

		"5">:: (fun () -> assert_equal (Int64.mul 12L 1024L) (M.atoix 0 (!!!"12K")));
		"6">:: (fun () -> assert_equal (Int64.mul (-12L) (Int64.mul 1024L 1024L)) (M.atoix 2 (!!!"  -12M")));

		"7">:: (fun () -> assert_equal 12.345 (M.atof 0 (!!!"12.345")));
		"8">:: (fun () -> assert_equal 123.45 (M.atof 1 (!!!"A123.45")));
		"9">:: (fun () -> assert_equal 12345.0 (M.atof 2 (!!!"AB12345")));
		"10">:: (fun () -> assert_raises (Invalid_argument "kcstring.atof") (fun() -> (M.atof 5 (!!!"12.34"))));
	];
	
	"misc">:::[
		"1">:: (fun() -> assert_equal 0L (M.levdist (!!!"abc") (!!!"abc") false));
		"2">:: (fun() -> assert_equal 1L (M.levdist (!!!"abcdefg") (!!!"abcXefg") false));
		"3">:: (fun() -> assert_equal 2L (M.levdist (!!!"abcdefg") (!!!"abcXefX") false));
		"4">:: (fun() -> assert_equal 0L (M.levdist_from_string (!!!"abc") ("abc") false));
		"5">:: (fun() -> assert_equal 1L (M.levdist_from_string (!!!"abcdefg") ("abcXefg") false));
		"6">:: (fun() -> assert_equal 2L (M.levdist_from_string (!!!"abcdefg") ("abcXefX") false));

		"7">:: (fun() -> assert_equal true (M.hashmurmur (!!!"abcdef") <> M.hashmurmur (!!!"abcdeg")));
		"9">:: (fun() -> assert_equal true (M.hashfnv (!!!"abcdef") <> M.hashfnv (!!!"abcdeg")));
	];
	
	"Gc">:: (fun () -> assert_equal () (Gc.full_major()));
];;

let _ = 
  print_endline "\n>>> Unit Test KCString <<<";
  run_test_tt_main kcstringT
;;



