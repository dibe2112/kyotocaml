(*
 * kcdbm - a replacement for module CamlDBM, using kyoto cabinet 
 *                  as database backend; based on the kyotocaml binding
 *
 * Copyright (C) 2013   D. Beutner  dbe dot mailbox at gmail dot com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* --------------------------------------------------------------------------*)
module type DBM_SUFFIX = sig val fname: string -> exn -> string end

module Dbm =
functor (Sfx: DBM_SUFFIX) -> struct

type tDb
type tCursor

type t = tDb * (tCursor option ref)

type open_flag =
   Dbm_rdonly | Dbm_wronly | Dbm_rdwr | Dbm_create |
   Dbm_trunc | Dbm_autosync 
   

type t_open_mode = 
  KCOWRITER | KCOREADER 
| KCOCREATE | KCOTRUNCATE | KCOAUTOTRAN | KCOAUTOSYNC 
| KCONOLOCK | KCOTRYLOCK  | KCONOREPAIR 

exception Dbm_error of string

external _make: unit -> tDb option = "caml_kcdb_make"
external _open : tDb -> string -> t_open_mode list -> int -> bool = "caml_kcdb_open" 
external _close : tDb -> unit = "caml_kcdb_close"
external _replace : tDb -> string -> string -> bool = "caml_kcdb_replace_string"
external _remove : tDb -> string -> bool = "caml_kcdb_remove"
(* external _append : tDb -> string -> string -> bool = "caml_kcdb_append_string" *)
external _add: tDb -> string -> string -> bool = "caml_kcdb_add_string"
(* external _check: tDb -> string -> int option = "caml_kcdb_check" *)
external _get : tDb -> string -> string option = "caml_kcdb_get_as_string"
external _set : tDb -> string -> string -> bool = "caml_kcdb_set_string"
external _cursor : tDb -> tCursor option = "caml_kcdb_cursor"
external _cursorx : tDb -> tCursor = "caml_kcdb_cursor_exc"
external _getkey : tCursor -> bool -> string option= "caml_kccur_getkey_as_string"
external _jump: tCursor -> bool = "caml_kccur_jump"
external _jumpx: tCursor -> unit = "caml_kccur_jump_exc"
external _curgetx : tCursor -> bool -> string * string = "caml_kccur_get_as_string_exc"
external _step: tCursor -> bool = "caml_kccur_step"

let __opendbm file flags mode =
  let dbopt = _make () in
  match dbopt with
  | None -> raise(Dbm_error("Can't open file " ^ file))
  | Some db -> 
    if (_open db file flags mode) = false then 
      raise(Dbm_error("Can't open file " ^ file))
    else
      (db, ref None)

let _opendbm file flags mode =
   let mapflags flag =
   match flag with
   | Dbm_rdonly -> [KCOREADER] 
   | Dbm_wronly -> [KCOWRITER]
   | Dbm_rdwr -> [KCOREADER; KCOWRITER] 
   | Dbm_create -> [KCOCREATE]
   | Dbm_trunc -> [KCOTRUNCATE]
   | Dbm_autosync -> [KCOAUTOSYNC]
   in __opendbm file ( List.flatten(List.map mapflags flags) ) mode

let opendbm file flags mode = 
   _opendbm (Sfx.fname file (Dbm_error("Can't open file " ^ file))) flags mode

let close (db,cur) = cur := None; ignore(_close db)

let find (db, _) key = 
   match _get db key with
   | Some value -> value
   | None -> raise Not_found

(*
let add (db, _) key value =
   if (_check db key) <> None then 
	raise (Dbm_error "Entry already exists")
   else if _append db key value then () 
   else raise (Dbm_error "dbm_store failed")
*)

let add (db, _) key value = 
	if _add db key value then () 
	else raise (Dbm_error "dbm_store failed")


let replace (db,_) key value = 
   if _set db key value then () 
   else raise (Dbm_error "dbm_store failed") 


let remove t k =
	if _remove (fst t) k then () else raise (Dbm_error "dbm_store failed")


let rec firstkey (db,cur) =
	match !cur with 
	| Some crsr -> begin
		if false = (_jump crsr) then raise Not_found
		else match _getkey crsr false with
		| Some s -> s
		| None -> raise Not_found
		end
	| None -> cur := _cursor db; 
		if !cur = None then raise Not_found 
		else firstkey (db,cur)



let nextkey (db,cur) =
	match !cur with 
	| Some crsr -> begin
		if (_step crsr) = false then
			raise Not_found
		else
		match _getkey crsr false with
		| Some s -> s
		| None -> raise Not_found
		end
	| None -> raise Not_found 

(*
let iter f t =
  let rec walk = function
      None -> ()
    | Some k ->
        f k (find t k);
        walk (try Some(nextkey t) with Not_found -> None)
  in
  walk (try Some(firstkey t) with Not_found -> None)
*)

(*
let rec iter f dc =
  let (db,cur) = dc in 
  if !cur <> None then begin
	let rec walk = function
	None -> ()
	| Some k -> 
		f k (find (db,cur) k); 
		walk (try Some(nextkey (db,cur)) with Not_found -> None)
	in
	walk (try Some(firstkey (db,cur)) with Not_found -> None)
  end else match _cursor db with
  | Some cr -> cur := Some cr;  iter f dc)
  | None -> ()
*)

let rec iter f dc =
  let loop c = 
    _jumpx c;
    while true do
    let (k,v) = _curgetx c true in
      f k v
    done
  in
  let (db,cur) = dc in
  try
    match !cur with
    |None -> begin
        let c =  _cursorx db in
	(snd dc) := Some c;
	loop c;
    end
    | Some c -> loop c
  with
  | _ -> ()

external _begintran: tDb -> bool -> bool = "caml_kcdb_begintran"
external _begintrantry: tDb -> bool -> bool = "caml_kcdb_begintrantry"

external _endtran: tDb -> bool -> bool = "caml_kcdb_endtran"

let begintran(db,_) = _begintran db
let begintrantry(db,_) = _begintrantry db

let commit (db,_) = _endtran db true
let rollback (db,_) = _endtran db false

external _dump: tDb -> string -> bool = "caml_kcdb_dumpsnap"
external _load: tDb -> string -> bool = "caml_kcdb_loadsnap"

let dump (db,_) s = _dump db s
let load s (db,_)  = _load db s

external _sync: tDb -> bool -> int -> int -> bool = "caml_kcdb_sync"

let sync (db,_) hard = _sync db hard 0 0

end

module HASH_SUFFIX = struct
  let fname file ex = 
  match file with
  | ":" | "*" -> file
  | "+" | "-" | "%" -> raise ex
  | _ -> file ^ ".kch"
end

module TREE_SUFFIX = struct
  let fname file ex = 
  match file with
  | "+" | "-" -> file
  | "*" | ":" | "%" -> raise ex
  | _ -> file ^ ".kct"
end

module Hash = Dbm(HASH_SUFFIX) 
module Tree = Dbm(TREE_SUFFIX) 
