(*
 * kyotocaml - ocaml binding for the kyoto cabinet database library
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


external version: unit -> string = "kc_version" 


module KCString = struct

type t

external create: int -> t = "caml_kcstring_create"
external make: int -> char -> t = "caml_kcstring_make"
external copy: t -> t = "caml_kcstring_copy"
external enlarge: t -> int -> unit = "caml_kcstring_enlarge"

external of_string: string -> t = "caml_kcstring_of_string"
external to_string: t -> string = "caml_kcstring_to_string"

external of_int: int -> t = "caml_kcstring_of_int"
external of_int64: Int64.t -> t = "caml_kcstring_of_int64"
external of_int32: Int32.t -> t = "caml_kcstring_of_int32"
external of_intnat: Nativeint.t -> t = "caml_kcstring_of_intnat"
external of_float: float -> int -> t = "caml_kcstring_of_float"

external compare: t -> t -> int = "caml_kcstring_compare"
external compare_nocase: t -> t -> int = "caml_kcstring_compare_nocase"
external compare_with_string: t -> string -> int = "caml_kcstring_compare_with_string"
external compare_nocase_with_string: t -> string -> int = "caml_kcstring_compare_nocase_with_string"

external length: t -> int = "caml_kcstring_length"

external unsafe_get: t -> int -> char = "caml_kcstring_unsafe_get"
external unsafe_set: t -> int -> char -> unit = "caml_kcstring_unsafe_set"
external get: t -> int -> char = "caml_kcstring_get"
external set: t -> int -> char -> unit = "caml_kcstring_set"

external sub: t -> int -> int -> t = "caml_kcstring_sub"
external sub_as_string: t -> int -> int -> string = "caml_kcstring_sub_as_string"

external concat: t list -> t -> t = "caml_kcstring_concat"
external concats: t list -> string -> t = "caml_kcstring_concat"
external concat_to_string: t list -> t -> string = "caml_kcstring_concat_to_string"

external rawfind: int -> t -> t -> int = "caml_kcstring_find"
external rawfind_string: int -> t -> string -> int = "caml_kcstring_find_string"
external rawfind_in_string: int -> string -> t -> int = "caml_kcstring_find_in_string"
external rawfind_nocase: int -> t -> t -> int = "caml_kcstring_find_nocase"
external rawfind_nocase_string: int -> t -> string -> int = "caml_kcstring_find_nocase_string"
external rawfind_nocase_in_string: int -> string -> t -> int = "caml_kcstring_find_nocase_in_string"

let find ?(offset=0) s1 s2 =
	let n = rawfind offset s1 s2 in 
	if n < 0 then raise Not_found else n

let find_string ?(offset=0) s1 s2 =
	let n = rawfind_string offset s1 s2 in 
	if n < 0 then raise Not_found else n

let find_in_string ?(offset=0) s1 s2 =
	let n = rawfind_in_string offset s1 s2 in 
	if n < 0 then raise Not_found else n

let find_nocase ?(offset=0) s1 s2 =
	let n = rawfind_nocase offset s1 s2 in 
	if n < 0 then raise Not_found else n

let find_nocase_string ?(offset=0) s1 s2 =
	let n = rawfind_nocase_string offset s1 s2 in 
	if n < 0 then raise Not_found else n

let find_nocase_in_string ?(offset=0) s1 s2 =
	let n = rawfind_nocase_in_string offset s1 s2 in 
	if n < 0 then raise Not_found else n

external print: t -> unit = "caml_kcstring_print"
external println: t -> unit = "caml_kcstring_println"

external atoi: int -> t -> Int64.t = "caml_kcstring_atoi"
external atoix: int -> t -> Int64.t = "caml_kcstring_atoix"
external atof: int -> t -> float = "caml_kcstring_atof"

external levdist: t -> t -> bool -> Int64.t = "caml_kcstring_levdist"
external levdist_from_string: t -> string -> bool -> Int64.t = "caml_kcstring_levdist_from_string"
external hashmurmur: t -> Int64.t = "caml_kcstring_hashmurmur"
external hashfnv: t -> Int64.t = "caml_kcstring_hashfnv"

module Op = struct
	external ( ^::: ): t -> t -> t = "caml_kcstring_append"
	external ( ^::. ): t -> t -> string = "caml_kcstring_append_to_string"

	let ( ~~ ) = of_string
	let ( ~> ) = to_string
end

end (* KCString *)

(*********************************************************************
*  Misc                                                              *
*********************************************************************)

module Misc = struct

external levdist: string -> string -> bool -> Int64.t = "caml_kcstring_levdist_of_strings"
external hashmurmur: string -> Int64.t = "caml_kcstring_hashmurmur_of_string"
external hashfnv: string -> Int64.t = "caml_kcstring_hashfnv_of_string"

end (* Misc *)

(*********************************************************************
*  Openparam                                                         *
*********************************************************************)

module Openparam = struct

type t_log = [`Path of string | `Stdout | `Stderr | `None]

let log : t_log -> string =
	function 
	| `Path(s) -> "#log=" ^ s
	| `Stdout -> "#log=-"
	| `Stderr -> "#log=+"
	| `None -> ""

type t_logkinds = [`Debug | `Info | `Warn | `Error 
	| `Kinds of t_logkinds list | `All | `None]

let rec logkinds : t_logkinds -> string =
	function 
	| `Debug -> "#logkinds=debug"
	| `Info -> "#logkinds=info"
	| `Warn -> "#logkinds=warn"
	| `Error -> "#logkinds=error"
	| `Kinds(`All::t) -> (logkinds (`Kinds t))
	| `Kinds((`Kinds _)::t) -> (logkinds (`Kinds t))
	| `Kinds(h::t) -> (logkinds h) ^ (logkinds (`Kinds t))
	| `Kinds([]) -> ""  
	| `All -> "#logkinds=debug#logkinds=info#logkinds=warn#logkinds=error"
	| `None -> ""

type t_logpx = [`Px of string | `None]

let logpx : t_logpx -> string =
	function 
	| `Px(s) -> "#logpx=" ^ s
	| `None -> ""

type t_opts = [`Small | `Linear | `Compress | 
	`SmallLin | `SmallCompr | `LinCompr | `All | `None]

let opts : t_opts -> string =
	function
	| `Small -> "#opts=s" 
	| `Linear -> "#opts=l" 
	| `Compress -> "#opts=c"
	| `SmallLin -> "#opts=s#opts=l"
	| `SmallCompr -> "#opts=s#opts=c"
	| `LinCompr -> "#opts=l#opts=c"
	| `All -> "#opts=s#opts=l#opts=c"
	| `None -> ""

type t_int = [ `I of int | `None ]
type t_long = [ `I64 of int64 | `None ]

let bnum : t_long -> string =
	function 
	| `I64(n) -> "#bnum=" ^ (Int64.to_string n) 
	| `None -> ""

type t_zcmp = [ `Zlib | `Def | `Gz | `Lzo | `Lzma | `Arc | `None ]

let zcmp : t_zcmp -> string = 
	function 
	| `Zlib -> "#zcmp=zlib"
	| `Def -> "#zcmp=def"
	| `Gz -> "#zcmp=gz"
	| `Lzo -> "#zcmp=lzo"
	| `Lzma -> "#zcmp=lzma"
	| `Arc -> "#zcmp=arc"
	| `None -> ""

let capcnt : t_long -> string = 
	function 
	| `I64(n) -> "#capcnt=" ^ (Int64.to_string n) 
	| `None -> ""

let capsiz : t_long -> string = 
	function 
	| `I64(n) -> "#capsiz=" ^ (Int64.to_string n) 
	| `None -> ""

let zkey : t_long -> string =
	function 
	| `I64(n) -> "#zkey=" ^ (Int64.to_string n)
	| `None -> ""

let psiz : t_long -> string = 
	function 
	| `I64(n) -> "#psiz=" ^ (Int64.to_string n)
	| `None -> ""

type t_rcomp = [`Lex | `Dec | `None ]

let rcomp : t_rcomp -> string = 
	function 
	| `Lex -> "#rcomp=lex" 
	| `Dec -> "#rcomp=dec" 
	| `None -> ""

let pccap : t_long -> string =
	function 
	| `I64(n) -> "#pccap=" ^  (Int64.to_string n)
	| `None -> ""

let apow : t_int -> string =
	function 
	| `I(n) -> "#apow=" ^  (string_of_int n)
	| `None -> ""

let fpow : t_int -> string =
	function 
	| `I(n) -> "#fpow=" ^  (string_of_int n)
	| `None -> ""

let msiz : t_long -> string = 
	function 
	| `I64(n) -> "#msiz=" ^ (Int64.to_string n)
	| `None -> ""

let dfunit : t_long -> string =
	function 
	| `I64(n) -> "#dfunit="  ^ (Int64.to_string n)
	| `None -> ""


let path_file_x p n suff = (Filename.concat p n) ^ suff

end (* Openparam *)


module OpenDb = struct
include Openparam

let open_proto_hash_db
	open_func
	db 
	?(log=`None) 
	?(logkinds=`None) 
	?(logpx=`None) 
	mode = 
	let l = "-"
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::[] in
	let p = String.concat "" l in
	open_func db p mode

let open_proto_tree_db
	open_func
	db
	?(log=`None) 
	?(logkinds=`None) 
	?(logpx=`None) 
	mode = 
	let l = "+"
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::[] in
	let p = String.concat "" l in
	open_func db p mode

let open_stash_db 
	open_func
	db
	?(log=`None) 
	?(logkinds=`None) 
	?(logpx=`None) 
	?(bnum=`None) 
	mode = 
	let l = ":"
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.bnum bnum)
	::[] in
	let p = String.concat "" l in
	open_func db p mode

let open_cache_hash_db
	open_func
	db 
	?(log=`None) 
	?(logkinds=`None) 
	?(logpx=`None) 
	?(opts=`None) 
	?(bnum=`None) 
	?(zcmp=`None) 
	?(capcnt=`None) 
	?(capsiz=`None) 
	?(zkey=`None) 
	mode = 
	let l = "*"
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.opts opts)
	::(Openparam.bnum bnum)
	::(Openparam.zcmp zcmp)
	::(Openparam.capcnt capcnt)
	::(Openparam.capsiz capsiz)
	::(Openparam.zkey zkey)
	::[] in
	let p = String.concat "" l in
	open_func db p mode


let open_cache_tree_db
	open_func
	db
	?(log=`None)
	?(logkinds=`None)
	?(logpx=`None) 
	?(opts=`None)
	?(bnum=`None)
	?(zcmp=`None)
	?(capcnt=`None)
	?(capsiz=`None)
	?(zkey=`None)
	?(psiz=`None)
	?(rcomp=`None)
	?(pccap=`None) 
	mode =
	let l = "%"
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.opts opts)
	::(Openparam.bnum bnum)
	::(Openparam.zcmp zcmp)
	::(Openparam.capcnt capcnt)
	::(Openparam.capsiz capsiz)
	::(Openparam.zkey zkey)
	::(Openparam.psiz psiz)
	::(Openparam.rcomp rcomp)
	::(Openparam.pccap pccap)
	::[] in
	let p = String.concat "" l in
	open_func db p mode

let open_file_hash_db
	open_func
	db 
	?(log=`None) 
	?(logkinds=`None)
	?(logpx=`None) 
	?(apow=`None)
	?(fpow=`None)
	?(opts=`None)
	?(bnum=`None)
	?(msiz=`None)
	?(dfunit=`None)
	?(zcmp=`None)
	?(zkey=`None)
	p n 
	mode = 
	let l = (Openparam.path_file_x p n ".kch")
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.apow apow)
	::(Openparam.fpow fpow)
	::(Openparam.opts opts)
	::(Openparam.bnum bnum)
	::(Openparam.msiz msiz)
	::(Openparam.dfunit dfunit)
	::(Openparam.zcmp zcmp)
	::(Openparam.zkey zkey)
	::[] in
	let p = String.concat "" l in
	open_func db p mode

let open_file_tree_db
	open_func
	db
	?(log=`None)
	?(logkinds=`None)
	?(logpx=`None)  
	?(apow=`None)
	?(fpow=`None)
	?(opts=`None)
	?(bnum=`None)
	?(msiz=`None)
	?(dfunit=`None)
	?(zcmp=`None)
	?(zkey=`None)
	?(psiz=`None)
	?(rcomp=`None)
	?(pccap=`None)
	p n 
	mode = 
	let l = (Openparam.path_file_x p n ".kct")
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.apow apow)
	::(Openparam.fpow fpow)
	::(Openparam.opts opts)
	::(Openparam.bnum bnum)
	::(Openparam.msiz msiz)
	::(Openparam.dfunit dfunit)
	::(Openparam.zcmp zcmp)
	::(Openparam.zkey zkey)
	::(Openparam.psiz psiz)
	::(Openparam.rcomp rcomp)
	::(Openparam.pccap pccap)
	::[] in
	let p = String.concat "" l in
	open_func db p mode

let open_file_tree_db
	open_func
	db
	?(log=`None)
	?(logkinds=`None)
	?(logpx=`None)  
	?(apow=`None)
	?(fpow=`None)
	?(opts=`None)
	?(bnum=`None)
	?(msiz=`None)
	?(dfunit=`None)
	?(zcmp=`None)
	?(zkey=`None)
	?(psiz=`None)
	?(rcomp=`None)
	?(pccap=`None)
	p n 
	mode = 
	let l = (Openparam.path_file_x p n ".kct")
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.apow apow)
	::(Openparam.fpow fpow)
	::(Openparam.opts opts)
	::(Openparam.bnum bnum)
	::(Openparam.msiz msiz)
	::(Openparam.dfunit dfunit)
	::(Openparam.zcmp zcmp)
	::(Openparam.zkey zkey)
	::(Openparam.psiz psiz)
	::(Openparam.rcomp rcomp)
	::(Openparam.pccap pccap)
	::[] in
	let p = String.concat "" l in
	open_func db p mode

let open_dir_hash_db
	open_func
	db
	?(log=`None)
	?(logkinds=`None)
	?(logpx=`None)
	?(opts=`None)
	?(zcmp=`None)
	?(zkey=`None)	 
	p n 
	mode =
	let l = 	(Openparam.path_file_x p n ".kcd")
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.opts opts)
	::(Openparam.zcmp zcmp)
	::(Openparam.zkey zkey)
	::[] in	 
	let p = String.concat "" l in
	open_func db p mode

let open_dir_tree_db
	open_func
	db
	?(log=`None)
	?(logkinds=`None)
	?(logpx=`None)  
	?(opts=`None)
	?(zcmp=`None)
	?(zkey=`None)	 
	?(psiz=`None)
	?(rcomp=`None)
	?(pccap=`None)
	p n 
	mode =
	let l = (Openparam.path_file_x p n ".kcf")
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::(Openparam.opts opts)
	::(Openparam.zcmp zcmp)
	::(Openparam.zkey zkey)
	::(Openparam.psiz psiz)
	::(Openparam.rcomp rcomp)
	::(Openparam.pccap pccap)
	::[] in
	let p = String.concat "" l in
	open_func db p mode


let open_text_db 
	open_func
	db
	?(log=`None)
	?(logkinds=`None)
	?(logpx=`None) 
	p n 
	mode =
	let l = (Openparam.path_file_x p n ".kcx")
	::(Openparam.log log)
	::(Openparam.logkinds logkinds)
	::(Openparam.logpx logpx)
	::[] in 
	let p = String.concat "" l in
	open_func db p mode

end (* OpenDb *)

(*********************************************************************
*  kcerrcode                                                         *
*********************************************************************)

module KCErrcode = struct

type t = KCESUCCESS | KCENOIMPL | KCEINVALID | KCENOREPOS 	
	| KCENOPERM | KCEBROKEN | KCEDUPREC | KCENOREC | KCELOGIC
	| KCESYSTEM | KCEMISC 	

end (* KCErrcode *)

(*********************************************************************
*  kcdbcommmon                                                       *
*********************************************************************)

module KCDbcommon = struct

exception Key_not_found of string
let _ = Callback.register_exception "kyotOCAml_Exc_KeyNotFound" (Key_not_found "")

type t_open_mode = 
  KCOWRITER | KCOREADER 
| KCOCREATE | KCOTRUNCATE | KCOAUTOTRAN | KCOAUTOSYNC 
| KCONOLOCK | KCOTRYLOCK  | KCONOREPAIR 

type 'a t_fileproc = 
	Fileproc of	(string -> Int64.t -> Int64.t -> 'a -> bool) 
	| Nullproc  

end (* KCDbcommon *)

(*********************************************************************
*  kccur                                                             *
*********************************************************************)

module KCCur = struct

type t

external ecode: t -> int = "caml_kccur_ecode"
external emsg: t -> string = "caml_kccur_emsg"

module Ret = struct
	external setvalue: t -> KCString.t -> bool -> bool = "caml_kccur_setvalue"
	external setvalue_string: t -> string -> bool -> bool = "caml_kccur_setvalue_string"

	external remove: t -> bool = "caml_kccur_remove"

	external getkey: t -> bool -> KCString.t option = "caml_kccur_getkey"
	external getkey_as_string: t -> bool -> string option = "caml_kccur_getkey_as_string"

	external getvalue: t -> bool -> KCString.t option = "caml_kccur_getvalue"
	external getvalue_as_string: t -> bool -> string option = "caml_kccur_getvalue_as_string"

	external get: t ->  bool -> (KCString.t * KCString.t) option = "caml_kccur_get"
	external get_as_string: t ->  bool -> (string * string) option= "caml_kccur_get_as_string"

	external seize: t -> (KCString.t * KCString.t ) option = "caml_kccur_seize"
	external seize_as_string: t -> (string * string) option = "caml_kccur_seize_as_string"

	external jump: t -> bool = "caml_kccur_jump"
	external jumpkey: t -> string -> bool = "caml_kccur_jumpkey"
	external jumpback: t -> bool = "caml_kccur_jumpback"

	external step: t -> bool = "caml_kccur_step"
	external stepback: t -> bool = "caml_kccur_stepback"

end (* KCCur.Ret *)

module Exc = struct
	external setvalue: t -> KCString.t -> bool -> unit = "caml_kccur_setvalue_exc"
	external setvalue_string: t -> string -> bool -> unit = "caml_kccur_setvalue_string_exc"

	external remove: t -> unit = "caml_kccur_remove_exc"

	external getkey: t -> bool -> KCString.t = "caml_kccur_getkey_exc"
	external getkey_as_string: t ->  bool -> string = "caml_kccur_getkey_as_string_exc"

	external getvalue: t ->  bool -> KCString.t = "caml_kccur_getvalue_exc"
	external getvalue_as_string: t -> bool -> string = "caml_kccur_getvalue_as_string_exc"

	external get: t -> bool -> KCString.t * KCString.t = "caml_kccur_get_exc"
	external get_as_string: t -> bool -> string * string = "caml_kccur_get_as_string_exc"

	external seize: t -> KCString.t * KCString.t = "caml_kccur_seize_exc"
	external seize_as_string: t -> string * string = "caml_kccur_seize_as_string_exc"

	external jump: t -> unit = "caml_kccur_jump_exc"
	external jumpkey: t -> string -> unit = "caml_kccur_jumpkey_exc"
	external jumpback: t -> unit = "caml_kccur_jumpback_exc"

	external step: t -> unit = "caml_kccur_step_exc"
	external stepback: t -> unit = "caml_kccur_stepback_exc"

end (* KCCur.Exc *)

end (* KCCur *)

(*********************************************************************
*  kcdb                                                              *
*********************************************************************)

module KCDb = struct

include Openparam
include KCDbcommon

type t

type t_merge_mode = KCMSET | KCMADD | KCMREPLACE | KCMAPPEND

type 'a t_visitfull_result =  KCVISNOP | KCVISREMOVE | KCVISRESULT of 'a
type 'a t_visitfull_func = KCString.t -> KCString.t -> 'a -> KCString.t t_visitfull_result
type 'a t_visitfull_func_string = string -> string -> 'a -> KCString.t t_visitfull_result

external ecode: t -> int = "caml_kcdb_ecode"
external emsg: t -> string = "caml_kcdb_emsg"
external path: t -> string = "caml_kcdb_path"

external dumpsnap: t -> string -> bool = "caml_kcdb_dumpsnap"
external loadsnap: t -> string -> bool = "caml_kcdb_loadsnap"

external cas: t  -> string -> KCString.t -> KCString.t -> bool = "caml_kcdb_cas"
external cas_string: t  -> string -> string -> KCString.t -> bool = "caml_kcdb_cas_string"
external cas_stringstring: t  -> string -> string -> string -> bool = "caml_kcdb_cas_stringstring"
	

external count: t -> Int64.t = "caml_kcdb_count"
external size: t -> Int64.t = "caml_kcdb_size"

external status: t -> string option = "caml_kcdb_status"

module Ret = struct

	external make: unit -> t option = "caml_kcdb_make"
	external open_db: t -> string -> t_open_mode list -> bool = "caml_kcdb_open"

	let open_proto_hash_db = OpenDb.open_proto_hash_db open_db 
	let open_proto_tree_db = OpenDb.open_proto_tree_db open_db 
	let open_stash_db = OpenDb.open_stash_db open_db 
	let open_cache_hash_db = OpenDb.open_cache_hash_db open_db 
	let open_cache_tree_db = OpenDb.open_cache_tree_db open_db 
	let open_file_hash_db = OpenDb.open_file_hash_db open_db 
	let open_file_tree_db = OpenDb.open_file_tree_db open_db 
	let open_dir_hash_db = OpenDb.open_dir_hash_db open_db 
	let open_dir_tree_db = OpenDb.open_dir_tree_db open_db 
	let open_text_db = OpenDb.open_text_db open_db 

	external close_db: t -> bool = "caml_kcdb_close"

	external iterate: t -> 'a t_visitfull_func -> 'a -> bool -> bool = "caml_kcdb_iterate"
	external iterate_string: t -> 'a t_visitfull_func_string -> 'a -> bool -> bool = "caml_kcdb_iterate_string"

	external set: t -> string -> KCString.t -> bool = "caml_kcdb_set"
	external set_string: t -> string -> string -> bool = "caml_kcdb_set_string"

	external add: t -> string -> KCString.t -> bool = "caml_kcdb_add"
	external add_string: t -> string -> string -> bool = "caml_kcdb_add_string"

	external replace: t -> string -> KCString.t -> bool = "caml_kcdb_replace"
	external replace_with_string: t -> string -> string -> bool = "caml_kcdb_replace_string"

	external append: t -> string -> KCString.t -> bool = "caml_kcdb_append"
	external append_string: t -> string -> string -> bool = "caml_kcdb_append_string"

	external incrint64: t -> string -> Int64.t -> Int64.t -> Int64.t = "caml_kcdb_incrint"
	external incrfloat: t -> string -> float -> float -> float = "caml_kcdb_incrdouble"
	let icnrdouble = incrfloat

	external remove: t -> string -> bool = "caml_kcdb_remove"

	external check: t -> string -> int option = "caml_kcdb_check"

	external get: t -> string -> KCString.t option = "caml_kcdb_get"
	external get_as_string: t -> string -> string option = "caml_kcdb_get_as_string"

	external seize: t -> string -> KCString.t option = "caml_kcdb_seize"
	external seize_as_string: t -> string -> string option = "caml_kcdb_seize_as_string"

	external get_buf: t -> string -> string -> int = "caml_kcdb_getbuf"

	external sync: t -> bool -> 'a t_fileproc -> 'a -> bool = "caml_kcdb_sync"
	
	external occupy: t -> bool -> (string -> Int64.t -> Int64.t -> 'a) -> 'a -> bool = "caml_kcdb_occupy" 
	external copy: t -> string -> bool = "caml_kcdb_copy"

	external begintran: t -> bool -> bool = "caml_kcdb_begintran"
	external begintrantry: t -> bool -> bool = "caml_kcdb_begintrantry"
	external endtran: t -> bool -> bool = "caml_kcdb_endtran"
	
	let commit db = endtran db true
	let rollback db = endtran db false

	external matchprefix: t -> string -> int -> string array option = "caml_kcdb_matchprefix"
	external matchregex: t -> string -> int -> string array option = "caml_kcdb_matchregex"
	external matchsimilar: t -> string -> int -> bool -> int -> string array option = "caml_kcdb_matchsimilar"

	external clear: t -> bool = "caml_kcdb_clear"
	external merge: t -> t array -> t_merge_mode -> bool = "caml_kcdb_merge"
	external cursor: t -> KCCur.t option = "caml_kcdb_cursor"

	external setsub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcdb_setsub"
	external settail: t -> string -> KCString.t -> int -> bool = "caml_kcdb_settail"
	external addsub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcdb_addsub"
	external addtail: t -> string -> KCString.t -> int -> bool = "caml_kcdb_addtail"
	external replace_with_sub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcdb_replace_with_sub"
	external replace_with_tail: t -> string -> KCString.t -> int -> bool = "caml_kcdb_replace_with_tail"
	external appendsub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcdb_appendsub"
	external appendtail: t -> string -> KCString.t -> int -> bool = "caml_kcdb_appendtail"

	external setbulk: t -> (string * KCString.t) list -> bool -> int option = "caml_kcdb_setbulk"
	external setbulk_string: t -> (string * string) list -> bool -> int option = "caml_kcdb_setbulk_string"
	external removebulk: t -> string list -> bool -> int option = "caml_kcdb_removebulk"
	external getbulk: t -> string list -> bool -> (string * KCString.t) list option = "caml_kcdb_getbulk"
	external getbulk_as_string: t -> string list -> bool -> (string * string) list option = "caml_kcdb_getbulk_as_string"

end (* Ret *)

module Exc = struct
	external make: unit -> t = "caml_kcdb_make_exc"
	external open_db: t -> string -> t_open_mode list -> unit = "caml_kcdb_open_exc"

	let open_proto_hash_db = OpenDb.open_proto_hash_db open_db 
	let open_proto_tree_db = OpenDb.open_proto_tree_db open_db 
	let open_stash_db = OpenDb.open_stash_db open_db 
	let open_cache_hash_db = OpenDb.open_cache_hash_db open_db 
	let open_cache_tree_db = OpenDb.open_cache_tree_db open_db 
	let open_file_hash_db = OpenDb.open_file_hash_db open_db 
	let open_file_tree_db = OpenDb.open_file_tree_db open_db 
	let open_dir_hash_db = OpenDb.open_dir_hash_db open_db 
	let open_dir_tree_db = OpenDb.open_dir_tree_db open_db 
	let open_text_db = OpenDb.open_text_db open_db 

	external close_db: t -> unit = "caml_kcdb_close_exc"

	external iterate: t -> 'a t_visitfull_func -> 'a -> bool -> unit = "caml_kcdb_iterate_exc"
	external iterate_string: t -> 'a t_visitfull_func_string -> 'a -> bool -> unit = "caml_kcdb_iterate_string_exc"

	external set: t -> string -> KCString.t -> unit = "caml_kcdb_set_exc"
	external set_string: t -> string -> string -> unit = "caml_kcdb_set_string_exc"

	external add: t -> string -> KCString.t -> unit = "caml_kcdb_add_exc"
	external add_string: t -> string -> string -> unit = "caml_kcdb_add_string_exc"

	external replace: t -> string -> KCString.t -> unit = "caml_kcdb_replace_exc"
	external replace_with_string: t -> string -> string -> unit = "caml_kcdb_replace_string_exc"

	external append: t -> string -> KCString.t -> unit = "caml_kcdb_append_exc"
	external append_string: t -> string -> string -> unit = "caml_kcdb_append_string_exc"

	external incrint64: t -> string -> Int64.t -> Int64.t -> Int64.t = "caml_kcdb_incrint_exc"
	external incrfloat: t -> string -> float -> float-> float = "caml_kcdb_incrdouble_exc"
	let icnrdouble = incrfloat

	external remove: t -> string -> unit = "caml_kcdb_remove_exc"

	external check: t -> string -> int = "caml_kcdb_check_exc"

	external get: t -> string -> KCString.t = "caml_kcdb_get_exc"
	external get_as_string: t -> string -> string = "caml_kcdb_get_as_string_exc"

	external sync: t -> bool -> 'a t_fileproc -> 'a -> unit = "caml_kcdb_sync_exc"

	external occupy: t -> bool -> (string -> Int64.t -> Int64.t -> 'a) -> 'a -> unit = "caml_kcdb_occupy_exc"

	external get_buf: t -> string -> string -> int = "caml_kcdb_getbuf_exc"

	external seize: t -> string -> KCString.t = "caml_kcdb_seize_exc"
	external seize_as_string: t -> string -> string = "caml_kcdb_seize_as_string_exc"

	external copy: t -> string -> unit = "caml_kcdb_copy_exc"

	external begintran: t -> bool -> unit = "caml_kcdb_begintran_exc"
	external begintrantry: t -> bool -> unit = "caml_kcdb_begintrantry_exc"
	external endtran: t -> bool -> unit = "caml_kcdb_endtran_exc"
	let commit db = endtran db true
	let rollback db = endtran db false

	external matchprefix: t -> string -> int -> string array = "caml_kcdb_matchprefix_exc"
	external matchregex: t -> string -> int -> string array = "caml_kcdb_matchregex_exc"
	external matchsimilar: t -> string -> int -> bool -> int -> string array =  "caml_kcdb_matchsimilar_exc"

	external clear: t -> unit = "caml_kcdb_clear_exc"
	external merge: t -> t array -> t_merge_mode -> unit = "caml_kcdb_merge_exc"
	external cursor: t -> KCCur.t = "caml_kcdb_cursor_exc"

	external setsub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcdb_setsub_exc"
	external settail: t -> string -> KCString.t -> int -> unit = "caml_kcdb_settail_exc"
	external addsub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcdb_addsub_exc"
	external addtail: t -> string -> KCString.t -> int -> unit = "caml_kcdb_addtail_exc"
	external replace_with_sub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcdb_replace_with_sub_exc"
	external replace_with_tail: t -> string -> KCString.t -> int -> unit = "caml_kcdb_replace_with_tail_exc"
	external appendsub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcdb_appendsub_exc"
	external appendtail: t -> string -> KCString.t -> int -> unit = "caml_kcdb_appendtail_exc"

	external setbulk: t -> (string * KCString.t) list -> bool -> int = "caml_kcdb_setbulk_exc"
	external setbulk_string: t -> (string * string) list -> bool -> int = "caml_kcdb_setbulk_string_exc"
	external removebulk: t -> string list -> bool -> int = "caml_kcdb_removebulk_exc"
	external getbulk: t -> string list -> bool -> (string * KCString.t) list = "caml_kcdb_getbulk_as_string_exc"
	external getbulk_as_string: t -> string list -> bool -> (string * string) list = "caml_kcdb_getbulk_as_string_exc"

end (* Exc *)

end (* KCDb *)

(*********************************************************************
*  kcidx                                                             *
*********************************************************************)

module KCIdx = struct

include Openparam
include KCDbcommon

exception Key_not_found =  KCDb.Key_not_found 

type t

external ecode: t -> int = "caml_kcidx_ecode"
external emsg: t -> string = "caml_kcidx_emsg"

external count: t -> Int64.t = "caml_kcidx_count"
external size: t -> Int64.t = "caml_kcidx_size"

external status: t -> string option = "caml_kcidx_status"
external path: t -> string = "caml_kcidx_path"


external status: t -> string option = "caml_kcdb_status"

module Ret = struct
	external make: unit -> t option = "caml_kcidx_make"
	external open_db: t -> string -> t_open_mode list -> bool = "caml_kcidx_open"

	let open_proto_hash_db = OpenDb.open_proto_hash_db open_db 
	let open_proto_tree_db = OpenDb.open_proto_tree_db open_db 
	let open_stash_db = OpenDb.open_stash_db open_db 
	let open_cache_hash_db = OpenDb.open_cache_hash_db open_db 
	let open_cache_tree_db = OpenDb.open_cache_tree_db open_db 
	let open_file_hash_db = OpenDb.open_file_hash_db open_db 
	let open_file_tree_db = OpenDb.open_file_tree_db open_db 
	let open_dir_hash_db = OpenDb.open_dir_hash_db open_db 
	let open_dir_tree_db = OpenDb.open_dir_tree_db open_db 
	let open_text_db = OpenDb.open_text_db open_db 

	external close_db: t -> bool = "caml_kcidx_close"

	external set: t -> string -> KCString.t -> bool = "caml_kcidx_set"
	external set_string: t -> string -> string -> bool = "caml_kcidx_set_string"

	external add: t -> string -> KCString.t -> bool = "caml_kcidx_add"
	external add_string: t -> string -> string -> bool = "caml_kcidx_add_string"

	external replace: t -> string -> KCString.t -> bool = "caml_kcidx_replace"
	external replace_with_string: t -> string -> string -> bool = "caml_kcidx_replace_string"

	external append: t -> string -> KCString.t -> bool = "caml_kcidx_append"
	external append_string: t -> string -> string -> bool = "caml_kcidx_append_string"

	external remove: t -> string -> bool = "caml_kcidx_remove"

	external get: t -> string -> KCString.t option = "caml_kcidx_get"
	external get_as_string: t -> string -> string option = "caml_kcidx_get_as_string"

	external sync: t -> bool -> 'a t_fileproc -> 'a -> bool = "caml_kcidx_sync"

	external clear: t -> bool = "caml_kcidx_clear"

	external inner_db_operation: t -> (KCDb.t -> 'a) -> 'a option = "caml_kcidx_inner_db_op"

	external setsub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcidx_setsub"
	external settail: t -> string -> KCString.t -> int -> bool = "caml_kcidx_settail"
	external addsub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcidx_addsub"
	external addtail: t -> string -> KCString.t -> int -> bool = "caml_kcidx_addtail"
	external replace_with_sub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcidx_replace_with_sub"
	external replace_with_tail: t -> string -> KCString.t -> int -> bool = "caml_kcidx_replace_with_tail"
	external appendsub: t -> string -> KCString.t -> int -> int -> bool = "caml_kcidx_appendsub"
	external appendtail: t -> string -> KCString.t -> int -> bool = "caml_kcidx_appendtail"

end (* Ret *)

module Exc = struct
	external make: unit -> t = "caml_kcidx_make_exc"
	external open_db: t -> string -> t_open_mode list -> unit = "caml_kcidx_open_exc"

	let open_proto_hash_db = OpenDb.open_proto_hash_db open_db 
	let open_proto_tree_db = OpenDb.open_proto_tree_db open_db 
	let open_stash_db = OpenDb.open_stash_db open_db 
	let open_cache_hash_db = OpenDb.open_cache_hash_db open_db 
	let open_cache_tree_db = OpenDb.open_cache_tree_db open_db 
	let open_file_hash_db = OpenDb.open_file_hash_db open_db 
	let open_file_tree_db = OpenDb.open_file_tree_db open_db 
	let open_dir_hash_db = OpenDb.open_dir_hash_db open_db 
	let open_dir_tree_db = OpenDb.open_dir_tree_db open_db 
	let open_text_db = OpenDb.open_text_db open_db 

	external close_db: t -> unit = "caml_kcidx_close_exc"


	external set: t -> string -> KCString.t -> unit = "caml_kcidx_set_exc"
	external set_string: t -> string -> string -> unit = "caml_kcidx_set_string_exc"

	external add: t -> string -> KCString.t -> unit = "caml_kcidx_add_exc"
	external add_string: t -> string -> string -> unit = "caml_kcidx_add_string_exc"

	external replace: t -> string -> KCString.t -> unit = "caml_kcidx_replace_exc"
	external replace_with_string: t -> string -> string -> unit = "caml_kcidx_replace_string_exc"

	external append: t -> string -> KCString.t -> unit = "caml_kcidx_append_exc"
	external append_string: t -> string -> string -> unit = "caml_kcidx_append_string_exc"

	external remove: t -> string -> unit = "caml_kcidx_remove_exc"

	external get: t -> string -> KCString.t = "caml_kcidx_get_exc"
	external get_as_string: t -> string -> string = "caml_kcidx_get_as_string_exc"
	
	external sync: t -> bool -> 'a t_fileproc -> 'a -> unit = "caml_kcidx_sync_exc"
	external clear: t -> unit = "caml_kcidx_clear_exc"

	external inner_db_operation: t -> (KCDb.t -> 'a) -> 'a = "caml_kcidx_inner_db_op_exc"


	external setsub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcidx_setsub_exc"
	external settail: t -> string -> KCString.t -> int -> unit = "caml_kcidx_settail_exc"
	external addsub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcidx_addsub_exc"
	external addtail: t -> string -> KCString.t -> int -> unit = "caml_kcidx_addtail_exc"
	external replace_with_sub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcidx_replace_with_sub_exc"
	external replace_with_tail: t -> string -> KCString.t -> int -> unit = "caml_kcidx_replace_with_tail_exc"
	external appendsub: t -> string -> KCString.t -> int -> int -> unit = "caml_kcidx_appendsub_exc"
	external appendtail: t -> string -> KCString.t -> int -> unit = "caml_kcidx_appendtail_exc"

end (* Ret *)

end

