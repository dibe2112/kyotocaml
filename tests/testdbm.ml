open Unix;;
open OUnit;;
open Kcdbm;;

module H = Hash;;
module T = Tree;;

let _ = Random.init 1;;

let counter = ref 0;;

let iterfunc k _ = if k.[3] = '1' then print_endline k; incr counter;;

let runH () = 
  let dbfile = "./hdbm" in
  try Sys.remove (dbfile ^ ".kch") with _ -> ();
  let db = H.opendbm dbfile [H.Dbm_rdwr; H.Dbm_create] 0o644 in
  for i = 1 to 100 do
	let k = "Key" ^ (string_of_int i) in
	let v = "Value" ^ (string_of_int i) in
	Printf.printf "%s|%s\n%!" k v;
	try H.add db k v with H.Dbm_error("Entry already exists") -> print_endline "Entry already exists"
  done;
  for i = 1 to 100 do
	let k = "Key" ^ (string_of_int(Random.int 120)) in
	try Printf.printf "Key=%s ... found value %s\n%!" k (H.find db k) 
	with | _ -> Printf.printf "Key=%s ... fail\n%!" k
  done;
  H.iter iterfunc db;
  H.close db;
  Gc.full_major();
;;

let runT () = 
  let dbfile = "./tdbm" in
  try Sys.remove (dbfile ^ ".kct") with _ -> ();
  let db = T.opendbm dbfile [T.Dbm_rdwr; T.Dbm_create] 0o644 in
  for i = 1 to 100 do
	let k = "Key" ^ (string_of_int i) in
	let v = "Value" ^ (string_of_int i) in
	Printf.printf ".%!";
	try T.add db k v with T.Dbm_error("Entry already exists") -> print_endline "Entry already exists"
  done;
  Printf.printf "\n%!";
  for i = 1 to 100 do
	let k = "Key" ^ (string_of_int(Random.int 120)) in
	try Printf.printf "Key=%s ... found value %s\n%!" k (T.find db k) 
	with | _ -> Printf.printf "Key=%s ... fail\n%!" k
  done;
  T.close db;
  Gc.full_major();
;;

let _ = 
runH (); runT ()
;;