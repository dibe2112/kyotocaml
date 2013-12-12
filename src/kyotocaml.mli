(*
 * kyotocaml - ocaml binding for the kyoto cabinet database library
 *
 * Copyright (C) 2013   D. Beutner   dbe dot mailbox at gmail dot com
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
(** Main module *)
(** {5 What is Kyotocaml?} *)
(** {b Kyotocaml} is an {b OCaml} binding for the {b Kyoto Cabinet} library. 
{b Kyoto Cabinet} ({b KC}) is a database which stores data records as key-value-pairs.

{b For Kyoto Cabinet see also: }{{: http://fallabs.com/kyotocabinet/api/index.html } Main Page at Fallabs.com }
*)

(** {5 Content} *)

(** @return Kyoto Cabinet library version information *)
val version: unit -> string 

(** KCString: wrapper for value buffers allocated by  Kyoto Cabinet*)
module KCString : sig
(** 
{b Subsequent in descriptions type KCString.t is denoted as "kcstring"; 
the ocaml standard type is denoted as "string"} *)

(** {5 Why a new string-like type for Kyotocaml?} *)
(** Many Kyoto Cabinet functions return as a result a memory buffer which 
was internally allocated on the heap. To use this data within 
an ocaml program, it would be neccessary to convert it into an string, that 
means to allocate a second area of memory, copy the data and release the 
buffer. To avoid unnecessary copy-operations, kcstring was created as a wrapper for 
the buffer.

Many Kyoto Cabinet functions have more than one binding in Kyotocaml, some 
using kcstring, others string as argument/result. If only simple string 
processing needed and no special functions from ocaml libraries are used 
kcstring often is the better choice. (for example copying data from one 
database to another would be more efficient using kcstring)
 
 
 *)

(** {5 Content} *)
(** type of kcstring *)
type t

(** [t] can be used with the following functions from OCaml stdlib:
{ul {- Pervasives.compare}
{- Hashtbl.hash}
{- Marshal.from_..., Marshal.to_...}}
*)

(** [create len] creates a kcstring with undefined content  *)
val create: int -> t

(** [make len c] creates a kcstring and fill it with a char *)
val make: int -> char -> t

(** [copy kcs] creates a kcstring as copy of another *)
val copy: t -> t

(** [enlarge s newLength] increases the capacity of a kcstring. 
If newLength is positive, the old content is copied to the begin 
of the new buffer, if newLength is negative it is copied to the end.
@raise Invalid_argument "kcstring.enlarge" if newLength < old lenght
*)
val enlarge: t -> int -> unit

(** [of_string s] creates a kcstring from an string *)
val of_string: string -> t

(** [to_string kcs] creates an string from kcstring *)
val to_string: t -> string

(** [of_int i] converts an int to kcstring *)
val of_int: int -> t

(** [of_int64 l] converts a long to decimal kcstring *)
val of_int64: Int64.t -> t

(** [of_int32 s] converts a int32 to decimal kcstring *)
val of_int32: Int32.t -> t

(** [of_intnat n] converts a nativeint to decimal kcstring *)
val of_intnat: Nativeint.t -> t

(** [of_float f] converts a float to numeric kcstring *)
val of_float: float -> int -> t

(** [compare kcs1 kcs2] compares two kcstrings *)
val compare: t -> t -> int

(** [compare_nocase kcs1 kcs2] compares two kcstrings; not case sensitive *)
val compare_nocase: t -> t -> int

(** Like [compare], but second argument is an ocaml string *)
val compare_with_string: t -> string -> int

(** Like [compare_nocase], but second argument is an ocaml string *)
val compare_nocase_with_string: t -> string -> int

(** Capacity of a kcstring *)
val length: t -> int

(** [unsafe_get s n] retrieves the n-th char of s; without bounds checking *)
val unsafe_get: t -> int -> char

(** [unsafe_set s n c] modifies the n-th char of s to c; without bounds checking *)
val unsafe_set: t -> int -> char -> unit

(** [get s n] retrieves the n-th char of s
@raise Invalid_argument if n < 0 or n >= Length of s *)
val get: t -> int -> char

(** [set s n c] modifies the n-th char of s to c
@raise Invalid_argument if n < 0 or n >= Length of s *)
val set: t -> int -> char -> unit

(** [sub s index len] creates a kcstring which content is an excerpt from s 
@raise Invalid_argument "kcstring.sub" if  index < 0 or len < 0 or index + len > length of s *)
val sub: t -> int -> int -> t

(** [sub_as_string s index len] creates an string which content is an excerpt from s 
@raise Invalid_argument "kcstring.sub" if  index < 0 or len < 0 or index + len > length of s *)
val sub_as_string: t -> int -> int -> string

(** [concat list_of_strings seperator] concatenates a list of kcstrings, inserting a separator between each pair *)
val concat: t list -> t -> t

(** Like [concat], but seperator is of type string *)
val concats: t list -> string -> t

(** Like [concat], but result is of type string *)
val concat_to_string: t list -> t -> string

(** [rawfind offset s s2find] searches for s2find in s; begins search at index offset   
@return Success => The index where first occurence of searched string begins; 
otherwise => negative integer *) 
val rawfind: int -> t -> t -> int

(** [rawfind offset s s2find] searches for s2find in s; begins search at index offset   
@return Success => The index where first occurence of searched string begins; 
otherwise => negative integer  *) 
val rawfind_string: int -> t -> string -> int

(** [rawfind offset s s2find] searches for s2find in s; begins search at index offset   
@return Success => The index where first occurence of searched string begins; 
otherwise => negative integer *) 
val rawfind_in_string: int -> string -> t -> int

(** Like rawfind, but not case sensitive *)
val rawfind_nocase: int -> t -> t -> int

(** Like rawfind_string, but not case sensitive *)
val rawfind_nocase_string: int -> t -> string -> int

(** Like rawfind_in_string, but not case sensitive *)
val rawfind_nocase_in_string: int -> string -> t -> int

(** [find ~offset:o s s2find] searches for s2find in s; begins search at offset o (default value for offset is 0)  
@return The index where first occurence of searched string begins 
@raise Not_found if search failed *)
val find: ?offset:int -> t -> t -> int

(** [find ~offset:o s s2find] searches for s2find in s; begins search at offset o (default value for offset is 0)  
@return The index where first occurence of searched string begins 
@raise Not_found if search failed *)
val find_string: ?offset:int -> t -> string -> int

(** [find ~offset:o s s2find] searches for s2find in s; begins search at offset o (default value for offset is 0)  
@return The index where first occurence of searched string begins 
@raise Not_found if search failed *)
val find_in_string: ?offset:int -> string -> t -> int

(** Like find, but not case sensitive
@return The index where first occurence of searched string begins 
@raise Not_found if search failed *)
val find_nocase: ?offset:int -> t -> t -> int

(** Like find_string, but not case sensitive
@return The index where first occurence of searched string begins 
@raise Not_found if search failed *)
val find_nocase_string: ?offset:int -> t -> string -> int

(** Like find_in_string, but not case sensitive
@return The index where first occurence of searched string begins 
@raise Not_found if search failed *)
val find_nocase_in_string: ?offset:int -> string -> t -> int

(** output on stdout *)
val print: t -> unit

(** output with linefeed on stdout *)
val println: t -> unit

(** conversion to long integer *)
val atoi: int -> t -> Int64.t

(** converts a kcstring with an optional metric prefix  (trailing "K", "M", "G", "T", "P" or "E"; case insensitive) to long integer 
@see <http://fallabs.com/kyotocabinet/api/kclangc_8h.html#a585366f65f8332aa439e6b640823894f > Kyoto Cabinet Reference *)
val atoix: int -> t -> Int64.t

(** conversion to float *)
val atof: int -> t -> float

(** [levdist s1 s2] calculates Levenstein distance between s1 and s2*)
val levdist: t -> t -> bool -> Int64.t

(** [levdist_from_string s1 s2] calculates Levenstein distance between s1 and s2*)
val levdist_from_string: t -> string -> bool -> Int64.t

(** [hashmurmur s] calculates Murmur hash value *)
val hashmurmur: t -> Int64.t

(** [hashfnv s] calculates Fowler-Noll-Vo (FNV) hash value *)
val hashfnv: t -> Int64.t

module Op : sig

(** [s1 ^::: s2] concatenates s1 and s2 *)
	val ( ^::: ): t -> t -> t

(** [s1 ^::. s2] concatenates s1 and s2 *)
	val ( ^::. ): t -> t -> string

(** [~~ s]: short for: [of_string s] *)
	val ( ~~ ) : string -> t

(** [~> s]: short for: to_string s *)
	val ( ~> ) : t -> string
end

end (* KCString *)

(* ======================================================================= *)

(**  Miscellaneous functions of Kyoto Cabinet library*)
module Misc : sig

(** [levdist s1 s2] calculates Levenstein-distance betweenn s1 and s2*)
val levdist: string -> string -> bool -> Int64.t

(** [hashmurmur s] calculates Murmur-hash value of s*)
val hashmurmur: string -> Int64.t

(** [hashfnv s] calculates Fowler-Noll-Vo-(FNV-) hash value of s *)
val hashfnv: string -> Int64.t

end (* Misc *)

(* ======================================================================= *)

(** This module specifies types used for opening databases and index databases *)
module Openparam : sig
(** {b For detailed description: }
{{: http://fallabs.com/kyotocabinet/api/kclangc_8h.html#a464237fa818e4b70b15526a898b5c5f5} Kyoto Cabinet Reference: kcdbopen } 
*)

(** Specifies destination of logging messages *)
type t_log = [`Path of string | `Stdout | `Stderr | `None]

(** Specifies which classes of messages to log *) 
type t_logkinds = 
	[`Debug | `Info | `Warn | `Error 
	| `Kinds of t_logkinds list | `All | `None]

(** Specifies prefix of each log message *)
type t_logpx = [`Px of string | `None]

(** Specifies tuning option *)
type t_opts = 
	[`Small | `Linear | `Compress | 
	`SmallLin | `SmallCompr | `LinCompr | `All | `None]

type t_int = [ `I of int | `None ]

type t_long = [ `I64 of int64 | `None ]

(** Specifies data compression algoritm *)
type t_zcmp = [ `Zlib | `Def | `Gz | `Lzo | `Lzma | `Arc | `None ]

(** Specifies comperator *)
type t_rcomp = [`Lex | `Dec | `None ]

end (* Openparam *)

(** Database cursor type and operations 
@see < http://fallabs.com/kyotocabinet/api/kclangc_8h.html#a20e630cadb0b3518c13c9ca6cff60aed > Kyoto Cabinet API *)
module KCCur : sig

(** type of a cursor object *)
type t

(** Error code *)
val ecode: t -> int

(** Error message *)
val emsg: t -> string

module Ret : sig

(** [setvalue cur newValue next] modifies the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => The index where first occurence of searched string begins *)
	val setvalue: t -> KCString.t -> bool -> bool

(** [setvalue cur newValue next] modifies the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => The index where first occurence of searched string begins *)
	val setvalue_string: t -> string -> bool -> bool

(** [remove cur] removes the current record
@return Success => The index where first occurence of searched string begins *)
	val remove: t -> bool

(** [getkey cur next] retrieves the key of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => [Some key], otherwise => [None] *)
	val getkey: t -> bool -> KCString.t option
	
(** [getkey_as_string cur next] retrieves the key of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => [Some key], otherwise => [None] *)
	val getkey_as_string: t -> bool -> string option

(** [getvalue cur next] retrieves the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => [Some key], otherwise => [None] *)
	val getvalue: t -> bool -> KCString.t option

(** [getvalue_as_string cur next] retrieves the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => [Some key], otherwise => [None] *)
	val getvalue_as_string: t -> bool -> string option

(** [get cur next] retrieves key and value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => [Some (key,value)], otherwise => [None] *)
	val get: t -> bool -> (KCString.t * KCString.t) option

(** [get_as_string cur next] retrieves key and value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@return Success => [Some (key,value)], otherwise => [None] *)
	val get_as_string: t -> bool -> (string * string) option

(** [seize cur] removes the current record, returning its key and value
@return Success => [Some (key,value)], otherwise => [None] *)
	val seize: t -> (KCString.t * KCString.t) option

(** [seize_as_string cur] removes the current record, returning its key and value
@return Success => [Some (key,value)], otherwise => [None] *)
	val seize_as_string: t -> (string * string) option

(** [jump cur] positions the cursor on the first record 
@return Success => [true], otherwise => [false] *)
	val jump: t -> bool

(** [jumpkey cur k] positions the cursor on the record with the key [k]
@return Success => [true], otherwise => [false] *)
	val jumpkey: t -> string -> bool

(** [jumpback cur] positions the cursor on the last record 
@return Success => [true], otherwise => [false] *)
	val jumpback: t -> bool

(** [step cur] positions the cursor on the next record
@return Success => [true], otherwise => [false] *)
	val step: t -> bool

(** [stepback cur] positions the cursor on the previous record
@return Success => [true], otherwise => [false] *)
	val stepback: t -> bool

end (* Ret *)

module Exc : sig
(** [setvalue cur newValue next] modifies the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val setvalue: t -> KCString.t -> bool -> unit

(** [setvalue_string cur newValue next] modifies the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val setvalue_string: t -> string -> bool -> unit

(** [remove cur] removes the current record
if [next] is [true], [cur] subsequently moves to next record;
@raise Failue if error occured *)
	val remove: t -> unit

(** [getkey cur next] retrieves the key of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val getkey: t -> bool -> KCString.t

(** [getkey_as_string cur next] retrieves the key of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val getkey_as_string: t -> bool -> string

(** [getvalue cur next] retrieves the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val getvalue: t -> bool -> KCString.t

(** [getvalue_as_string cur next] retrieves the value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val getvalue_as_string: t -> bool -> string

(** [get cur next] retrieves key and value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val get: t -> bool -> KCString.t * KCString.t
	
(** [get_as_string cur next] retrieves key and value of the current record;
if [next] is [true], [cur] subsequently moves to next record;
@raise Failure if error occured *)
	val get_as_string: t -> bool -> string * string

(** [seize cur] removes the current record, returning its key and value
@raise Failure if error occured *)
	val seize: t -> KCString.t * KCString.t 

(** [seize_as_string cur] removes the current record, returning its key and value
@raise Failure if error occured *)
	val seize_as_string: t -> string * string

(** [jump cur] positions the cursor on the first record 
@raise Failure if error occured *)
	val jump: t -> unit

(** [jumpkey cur k] positions the cursor on the record with the key [k]
@raise Failure if error occured *)
	val jumpkey: t -> string -> unit

(** [jumpback cur] positions the cursor on the last record 
@raise Failure if error occured *)
	val jumpback: t -> unit

(** [step cur] positions the cursor on the next record
@raise Failure if error occured *)
	val step: t -> unit

(** [stepback cur] positions the cursor on the previous record
@raise Failure if error occured *)
	val stepback: t -> unit

end (* Exc *)

end (* KCCur *)





(** Database type and operations 
@see < http://fallabs.com/kyotocabinet/api/kclangc_8h.html#a20e630cadb0b3518c13c9ca6cff60aed > Kyoto Cabinet API *)
module KCDb : sig

exception Key_not_found of string

(** polymorphic database object *)
type t

(** {b For detailed description of opening modes and parameters: }
{{: http://fallabs.com/kyotocabinet/api/kclangc_8h.html#a464237fa818e4b70b15526a898b5c5f5} Kyoto Cabinet Reference: kcdbopen } 
*)

(** opening modes (used in functions open_xyz):
{ul {- [KCOWRITER] opens for writing}
{- [KCOREADER] opens for reading} 
{- [KCOCREATE] opens for writing; if not exists, database will be created}
{- [KCTRUNCATE] opens for writing; if exists, database will be emptied} 
{- [KCOAUTOTRAN] automatic transaction.}
{- [KCOAUTOSYNC] automatic synchronization}
{- [KCONOLOCK] opens database without locking}
{- [KCOTRYLOCK] locks database without blocking}
{- [KCONOREPAIR] opens database without auto repair }}*)
type t_open_mode = 
	KCOWRITER | KCOREADER 
	| KCOCREATE | KCOTRUNCATE | KCOAUTOTRAN | KCOAUTOSYNC 
	| KCONOLOCK | KCOTRYLOCK  | KCONOREPAIR 

type 'a t_fileproc = 
	Fileproc of (string -> Int64.t -> Int64.t -> 'a -> bool) 
	| Nullproc  

type 'a t_visitfull_result =  KCVISNOP | KCVISREMOVE | KCVISRESULT of 'a
type 'a t_visitfull_func = KCString.t -> KCString.t -> 'a -> KCString.t t_visitfull_result
type 'a t_visitfull_func_string = string -> string -> 'a -> KCString.t t_visitfull_result

(** {b See} {! Kyotocaml.KCDb.Ret.merge }, {! Kyotocaml.KCDb.Exc.merge } *)
type t_merge_mode = KCMSET | KCMADD | KCMREPLACE | KCMAPPEND

(** [ecode db] returns the code of last occured error that  *)
val ecode: t -> int

(** [emsg db] returns the message for the last error that occured *)
val emsg: t -> string

(** [status db] queries informations about the database
@return Success => Some , otherwise => [false] *)
val status: t -> string option

(** [dumpsnap db path] writes content of database to file
@return Success => [true], otherwise => [false] *)
val dumpsnap: t -> string -> bool

(** [loadsnap db path] writes content of (dump-)file to database
@return Success => [true], otherwise => [false] *)
val loadsnap: t -> string -> bool

(** [cas db key valOld valNew] performs compare-and-swap-algorithm on a record 
@return Success => [true], otherwise => [false] *)
val cas: t  -> string -> KCString.t -> KCString.t -> bool

(** [cas db key valOld valNew] performs compare-and-swap-algorithm on a record 
@return Success => [true], otherwise => [false] *)
val cas_string: t  -> string -> string -> KCString.t -> bool

(** [cas db key valOld valNew] performs compare-and-swap-algorithm on a record 
@return Success => [true], otherwise => [false] *)
val cas_stringstring: t  -> string -> string -> string -> bool

(** [count db] 
@return number of records in the database *)
val count: t -> Int64.t

(** [size db] 
@return size of database file (in bytes) *)
val size: t -> Int64.t

(** [path db] 
@return file system path of the database file *)
val path: t -> string

(** In Ret module the success/failure of a function is indicated by return value *)
module Ret : sig 

(** [make ()] creates a database object
@return Success => [Some db], otherwise => [None] *)
	val make: unit -> t option

(** [open_db db path mode] opens a database
@return Success => [true], otherwise => [false] 
@see < http://fallabs.com/kyotocabinet/api/kclangc_8h.html#a464237fa818e4b70b15526a898b5c5f5 > Kyoto Cabinet Reference: kcdbopen *)
	val open_db: t -> string -> t_open_mode list -> bool

(**  Opens a proto hash database 
@return Success => [true], otherwise => [false] *)
	val open_proto_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	bool

(** Opens a proto tree database 
@return Success => [true], otherwise => [false] *)
	val open_proto_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	bool

(** Opens a stash database (in-memory) 
@return Success => [true], otherwise => [false] *)
	val open_stash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?bnum:Openparam.t_long ->
	 t_open_mode list ->
	bool

(** Opens a cache hash database 
@return Success => [true], otherwise => [false] *)
	val open_cache_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	 t_open_mode list ->
	bool

(** Opens a file tree database 
@return Success => [true], otherwise => [false] *)
	val open_cache_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 t_open_mode list ->
	bool

(** Opens a file hash database 
@return Success => [true], otherwise => [false] *)
	val open_file_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** Opens a file tree database 
@return Success => [true], otherwise => [false] *)
	val open_file_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** [open_dir_tree_db db  ~log:lg ~logkinds:lk ~logpx:lpx ~opt:o ~zcmp:zc ~zkey:zk path name modelist] 
opens a directory hash database 
@return Success => [true], otherwise => [false] *)
	val open_dir_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** Opens a directory tree database 
@return Success => [true], otherwise => [false] *)
	val open_dir_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** Opens a text database
@return  Success => [true], otherwise => [false]  *)
	val open_text_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** Closes database db
@return Success => [true], otherwise => [false] *)
	val close_db: t -> bool

(** [iterate db iterFunc param writable] calls function iterFunc for each record of database.
@return Success => [true], otherwise => [false] *)
	val iterate: t -> 'a t_visitfull_func -> 'a -> bool -> bool

(** [iterate db iterFunc param writable] calls function iterFunc for each record of database.
@return Success => [true], otherwise => [false] *)
	val iterate_string: t -> 'a t_visitfull_func_string -> 'a -> bool -> bool

(** [set db key val] modifies the value associated with key [key]
@return Success => [true], otherwise => [false] *)
	val set: t -> string -> KCString.t -> bool

(** [set_string db key val] modifies the value associated with key [key]
@return Success => [true], otherwise => [false] *)
	val set_string: t -> string -> string -> bool

(** [add db key val] adds a record to the database db
@return Success => [true], otherwise => [false] *)
	val add: t -> string -> KCString.t -> bool

(** [add_string db key val] adds a record to the database db
@return Success => [true], otherwise => [false] *)
	val add_string: t -> string -> string -> bool

(** [replace db key val] changes the value of a record
@return Success => [true], otherwise => [false] *)
	val replace: t -> string -> KCString.t -> bool

(** [replace db key val] changes the value of a record
@return Success => [true], otherwise => [false] *)
	val replace_with_string: t -> string -> string -> bool

(** [append db key value]: if a record with the key [key] exists 
this function appends [value] to the existing value, otherwise it does the same
as [add key value]. 
@return Success => [true], otherwise => [false] *)
	val append: t -> string -> KCString.t -> bool

(** [append_string db key value]: if a record with the key [key] exists 
this function appends [value] to the existing value, otherwise it does the same
as [add_string db key value]. 
@return Success => [true], otherwise => [false] *)
	val append_string: t -> string -> string -> bool
	
(** [incrint64 db key num orig] adds an Int64.t to the numeric value of record 
associated with [key]. If no such record exists, a new record is created and its value is set to
[orig] *)
	val incrint64: t -> string -> Int64.t -> Int64.t -> Int64.t

(** [incrfloat db key num orig] adds an float to the numeric value of record 
associated with [key]. If no such record exists, a new record is created and its value is set to
[orig] *)
	val incrfloat: t -> string -> float -> float -> float

(** Same as incrfloat *)
	val icnrdouble: t -> string -> float -> float -> float

(** [remove db key] removes the record with key [key] from database
@return Success => [true], otherwise => [false] *)
	val remove: t -> string -> bool

(** [check db key] tests if a record with key [key] exists
@return Success => [Some len] where len is the lenght of the related value, otherwise => [None] *)
	val check: t -> string -> int option

(** [get db key] retrieves the value associated with key [key] 
@return Success => [Some v] where v is the value, otherwise => [None] *)
	val get: t -> string -> KCString.t option

(** [get_as_string db key] retrieves the value associated with key [key] 
@return Success => [Some v] where v is the value, otherwise => [None] *)
	val get_as_string: t -> string -> string option

(** [seize db key] retrieves the value associated with key [key] and then 
deletes the record. 
@return Success => [Some v] where v is the value, otherwise => [None] *)
	val seize: t -> string -> KCString.t option

(** [seize_as_string db key] deletes the record associated with key [key] 
returns it's value. 
@return Success => [Some v] where v is the value, otherwise => [None] *)
	val seize_as_string: t -> string -> string option

(** [get_buf db key buf] copies the value associated with key [key] to [buf]
@return: Failure -> a number less than 0, Success => number of bytes copied to buf; the number ist less or equal than length of buf *)
	val get_buf: t -> string -> string -> int

(** [sync db hard func] synchronizes a database with filesystem or device
@return Success => [true], otherwise => [false] *)
	val sync: t -> bool -> 'a t_fileproc -> 'a -> bool
	
	val occupy: t -> bool -> (string -> Int64.t -> Int64.t -> 'a) -> 'a -> bool

(** [copy db path] creates a copy of the database file
@return Success => [true], otherwise => [false] *)
	val copy: t -> string -> bool

(** [begintran db] starts a transaction
@return Success => [true], otherwise => [false] *)
	val begintran: t -> bool -> bool

(** [begintrantry db] tries to start a transaction
@return Success => [true], otherwise => [false] *)
	val begintrantry: t -> bool -> bool

(** [endtran db commit] ending a transaction; 
if commit is [true], all changes in database since 
transaction started are retained, otherwise changes are abandoned
@return Success => [true], otherwise => [false] *)
	val endtran: t -> bool -> bool

(** Same as [endtran db true]
@return Success => [true], otherwise => [false] *)
	val commit: t -> bool

(** Same as [endtran db false]
@return Success => [true], otherwise => [false] *)
	val rollback: t -> bool

(** [matchprefix db prefixKey max] retrieves values of records which keys
have the prefix [prefixKey].
@return Success => Some arrayOfValues, otherwise => None *)
	val matchprefix: t -> string -> int -> string array option

(** [matchregex db regexKey max] retrieves values of records which keys
have match the regular expression [regexKey].
@return Success => Some arrayOfValues, otherwise => None *)
	val matchregex: t -> string -> int -> string array option

(** [matchsimilar db key maxLevDist utf max] retrieves values of records which keys
have Levenstein distance from [Key] less or equal than [maxLevDist].
@return Success => Some arrayOfValues, otherwise => None *)
	val matchsimilar: t -> string -> int -> bool -> int -> string array option 

(** Removing all data from database: [clear db]
@return Success => [true], otherwise => [false] *)
	val clear: t -> bool

(** [merge dbDest dbSrcArray mode] merges multiple databases [dbSrcArray] into one [dbDest] 
@return Success => [true], otherwise => [false] *)
	val merge: t -> t array -> t_merge_mode -> bool

(** [cursor db] creates a cursor object on database db
@return Success => [Some cursorObject], otherwise => [None] *)
	val cursor: t -> KCCur.t option

(** [setsub db key  s begin len] like [set],  but only a substring of s is stored as value 
@return Success => true, otherwise => false *)
	val setsub: t -> string -> KCString.t -> int -> int -> bool

(** [settail db key  s begin] like [set],  but only the tail of s is stored as value 
@return Success => true, otherwise => false *)
	val settail: t -> string -> KCString.t -> int -> bool

(** [addsub db key  s begin len] like [add],  but only a substring of s is stored as value 
@return Success => true, otherwise => false *)
	val addsub: t -> string -> KCString.t -> int -> int -> bool

(** [addtail db key  s begin] like [add],  but only the tail of s is stored as value 
@return Success => true, otherwise => false *)
	val addtail: t -> string -> KCString.t -> int -> bool

(** [replacesub db key  s begin len] like [replace],  but only a substring of s is stored as value 
@return Success => true, otherwise => false *)
	val replace_with_sub: t -> string -> KCString.t -> int -> int -> bool

(** [replacetail db key  s begin] like [replace],  but only the tail of s is stored as value 
@return Success => true, otherwise => false *)
	val replace_with_tail: t -> string -> KCString.t -> int -> bool

(** [appendsub db key  s begin len] like [append],  but only a substring of s is stored as value 
@return Success => true, otherwise => false *)
	val appendsub: t -> string -> KCString.t -> int -> int -> bool

(** [appendtail db key  s begin] like [append],  but only the tail of s is stored as value 
@return Success => true, otherwise => false *)
	val appendtail: t -> string -> KCString.t -> int -> bool

	val setbulk: t -> (string * KCString.t) list -> bool -> int option
	val setbulk_string: t -> (string * string) list -> bool -> int option
	val removebulk: t -> string list -> bool -> int option
	val getbulk: t -> string list -> bool -> (string * KCString.t) list option
	val getbulk_as_string: t -> string list -> bool -> (string * string) list option

end (* Ret *)

(** In Exc module the failure of a function is indicated by an exception *)
module Exc : sig

(** Creating a database object
@raise Out_of_memory if no database object could be created *)
	val make: unit -> t

(**  Open a database: [open_db db path mode]
@raise Failure if error occured *)
	val open_db: t -> string -> t_open_mode list -> unit

(**  Open a proto hash database
@raise Failure if error occured *)
	val open_proto_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	unit

(**  Open a proto tree database
@raise Failure if error occured *)
	val open_proto_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	unit

(**  Open a proto tree database
@raise Failure if error occured *)
	val open_stash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?bnum:Openparam.t_long ->
	 t_open_mode list ->
	unit

(**  Open a cache hash database
@raise Failure if error occured *)
	val open_cache_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	 t_open_mode list ->
	unit

(**  Open a cache tree database
@raise Failure if error occured *)
	val open_cache_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 t_open_mode list ->
	unit

(**  Open a file hash database
@raise Failure if error occured *)
	val open_file_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(**  Open a file tree database
@raise Failure if error occured *)
	val open_file_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(**  Open a directory hash database
@raise Failure if error occured *)
	val open_dir_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(**  Open a directory tree database
@raise Failure if error occured *)
	val open_dir_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(** [open_text_db db ~log:lg ~logkinds:lk ~logpx:lpx path name modelist] opens a text database
@raise Failure if error occured *)
	val open_text_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(** [close_db db] closes a database
@raise Failure if error occured *)
	val close_db: t -> unit

(** [iterate db iterFunc param writable] calls function iterFunc for each record of database.
@raise Failure if error occured *)
	val iterate: t -> 'a t_visitfull_func -> 'a -> bool -> unit

(** [iterate db iterFunc param writable] calls function iterFunc for each record of database.
@raise Failure if error occured *)
	val iterate_string: t -> 'a t_visitfull_func_string -> 'a -> bool -> unit

(** [set db key val] modifies the value associated with key [key]
@raise Failure if error occured *)
	val set: t -> string -> KCString.t -> unit
	
(** [set_string db key val] modifies the value associated with key [key]
@raise Failure if error occured *)
	val set_string: t -> string -> string -> unit

(** [add db key val] adds a record to the database db
@raise Failure  if error occured *)
	val add: t -> string -> KCString.t -> unit

(** [add_string db key val] adds a record to the database db
@raise Failure if error occured *)
	val add_string: t -> string -> string -> unit

(** [replace db key val] changes the value of a record
@raise Failure if error occured *)
	val replace: t -> string -> KCString.t -> unit

(** [replace_with_string db key val] changes the value of a record
@raise Failure if error occured *)
	val replace_with_string: t -> string -> string -> unit

(** [append db key value]: if a record with the key [key] exists 
this function appends [value] to the existing value, otherwise it does the same
as [add key value]. 
@raise Failure if error occured *)
	val append: t -> string -> KCString.t -> unit

(** [append_string db key value]: if a record with the key [key] exists 
this function appends [value] to the existing value, otherwise it does the same
as [add key value]. 
@raise Failure if error occured *)
	val append_string: t -> string -> string -> unit

(** [incrint64 db key num orig] adds an Int64.t to the numeric value of record 
associated with [key]. If no such record exists, a new record is created and its value is set to
[orig] 
@raise Failure if error occured *)
	val incrint64: t -> string -> Int64.t -> Int64.t -> Int64.t
	
	
(** [incrfloat db key num orig] adds an Int64.t to the numeric value of record 
associated with [key]. If no such record exists, a new record is created and its value is set to
[orig] 
@raise Failure if error occured *)
	val incrfloat: t -> string -> float -> float -> float
	
(** Same as [incrfloat] *) 
	val icnrdouble: t -> string -> float -> float -> float

(** [remove db key] removes the record with key [key] from database
@raise Key_not_found if error occured *)
	val remove: t -> string -> unit

(** [check db key] tests if a record with key [key] exists
@return lenght of the related value
@raise Key_not_found if error occured *)
	val check: t -> string -> int

(** database retrieval: [get db key]  
@return the value that is associated with [key]
@raise Key_not_found if key was not found *)
	val get: t -> string -> KCString.t

(** database retrieval: [get_as_string db key]
@return the value that is associated with [key]
@raise Key_not_found if key was not found *)
	val get_as_string: t -> string -> string

(** [seize db key] deletes record associated with [key] and returns its value 
@return the value taht was associated with [key]
@raise Key_not_found if [key] was not found *)
	val seize: t -> string -> KCString.t

(** [seize db key]  deletes record associated with [key] and returns its value
@return the value that was associated with [key]
@raise Key_not_found if [key] was not found *)
	val seize_as_string: t -> string -> string

(** [get_buf db key buf] copies the value associated with [key] to [buf] 
@return the number of chars copied to s 
@raise Key_not_found if [key] was not found *)
	val get_buf: t -> string -> string -> int

(** [sync db hard func] synchronizes a database with filesystem or device
@raise Failure if error occured *)
	val sync: t -> bool -> 'a t_fileproc -> 'a -> unit
	
	val occupy: t -> bool -> (string -> Int64.t -> Int64.t -> 'a) -> 'a -> unit 

(** [copy db path] creates a copy of the database file
@raise Failure if error occured *)
	val copy: t -> string -> unit

(** [begintran db] starts a transaction
@raise Failure if error occured *)
	val begintran: t -> bool -> unit

(** [begintrantry db] starts a transaction
@raise Failure if error occured *)
	val begintrantry: t -> bool -> unit

(** [endtran db commit] ends a transaction
@raise Failure if error occured *)
	val endtran: t -> bool -> unit

(** Same as [endtran db true] 
@raise Failure if error occured *)
	val commit: t -> unit

(** Same as [endtran db false] 
@raise Failure if error occured *)
	val rollback: t -> unit

(** [matchprefix db prefixKey max] retrieves values of records which keys
have the prefix [prefixKey].
@return Array with found values
@raise Invalid_argument if [max] < 1
@raise Failure if error occured *)
	val matchprefix: t -> string -> int -> string array

(** [matchregex db regexKey max] retrieves values of records which keys
have match the regular expression [regexKey].
@return Array with found values
@raise Invalid_argument if [max] < 1
@raise Failure if error occured *)
	val matchregex: t -> string -> int -> string array

(** [matchsimilar db key maxLevDist utf max] retrieves values of records which keys
have Levenstein distance from [Key] less or equal than [maxLevDist].
@return Array with found values
@raise Invalid_argument if [max] < 1
@raise Failure if error occured *)
	val matchsimilar: t -> string -> int -> bool -> int -> string array 

(** Removing all records from database: [clear db]
@raise Failure if error occured *)
	val clear: t -> unit

(** [merge dbDest dbSrcArray mode] merges multiple databases [dbSrcArray] into one [dbDest] 
@raise Failure if error occured *)
	val merge: t -> t array -> t_merge_mode -> unit
	
(** Creating a cursor object for a database
@return cursor object of type KCCur.t
@raise Failure if no cursor object could be created *)
	val cursor: t -> KCCur.t

(** [setsub db key  s begin len] like [set],  but only a substring of s is stored as value
@raise Invalid_argument if begin or len is out of range
@raise Failure if other error occured *)
	val setsub: t -> string -> KCString.t -> int -> int -> unit

(** [settail db key  s begin] like [set],  but only the tail of s is stored as value 
@raise Invalid_argument if begin is out of range
@raise Failure if other error occured *)
	val settail: t -> string -> KCString.t -> int -> unit
	
(** [addsub db key  s begin len] like [add],  but only a substring of s is stored as value 
@raise Invalid_argument if begin or len is out of range
@raise Failure if other error occured *)
	val addsub: t -> string -> KCString.t -> int -> int -> unit

(** [addtail db key  s begin] like [add],  but only a substring of s is stored as value 
@raise Invalid_argument if begin is out of range
@raise Failure if other error occured *)
	val addtail: t -> string -> KCString.t -> int -> unit
	
(** [replacesub db key  s begin len] like [replace],  but only a substring of s is stored as value 
@raise Invalid_argument if begin or len is out of range
@raise Failure if other error occured *)
	val replace_with_sub: t -> string -> KCString.t -> int -> int -> unit

(** [replacetail db key  s begin] like [replace],  but only the tail of s is stored as value 
@raise Invalid_argument ["kcdb.replacetail"] if begin is out of range
@raise Failure if other error occured *)
	val replace_with_tail: t -> string -> KCString.t -> int -> unit

(** [appendsub db key  s begin len] like [append],  but only a substring of s is stored as value 
@raise Invalid_argument ["kcdb.appendsub"] if begin or len is out of range
@raise Failure if other error occured *)
	val appendsub: t -> string -> KCString.t -> int -> int -> unit

(** [appendtail db key  s begin] like [append],  but only the tail of s is stored as value 
@raise Invalid_argument if begin is out of range
@raise Failure if other error occured *)
	val appendtail: t -> string -> KCString.t -> int -> unit

	val setbulk: t -> (string * KCString.t) list -> bool -> int
	val setbulk_string: t -> (string * string) list -> bool -> int
	val removebulk: t -> string list -> bool -> int
	val getbulk: t -> string list -> bool -> (string * KCString.t) list
	val getbulk_as_string: t -> string list -> bool -> (string * string) list

end (* Exc *)

end (* KCDb *)

(* ======================================================================= *)

(** Index database *)
module KCIdx : sig

(** type of a (polymorphic) index database object *)
type t

type t_open_mode = KCDb.t_open_mode

exception Key_not_found of string (*=  KCDb.Key_not_found of string*)

type 'a t_fileproc = 
	Fileproc of (string -> Int64.t -> Int64.t -> 'a -> bool)
	| Nullproc  

(** {b See} {!Kyotocaml.KCDb.ecode} *)
val ecode: t -> int

(** {b See} {! Kyotocaml.KCDb.emsg} *)
val emsg: t -> string

(** {b See} {! Kyotocaml.KCDb.emsg} *)
val count: t -> Int64.t

(** {b See} {! Kyotocaml.KCDb.size} *)
val size: t -> Int64.t

(** {b See} {! Kyotocaml.KCDb.status} *)
val status: t -> string option

(** {b See} {! Kyotocaml.KCDb.path} *)
val path: t -> string

module Ret : sig
(** {b See} {! Kyotocaml.KCDb.Ret.make} *)
	val make: unit -> t option

(** {b See} {! Kyotocaml.KCDb.Ret.open_db} *)
	val open_db: t -> string -> t_open_mode list -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_proto_hash_db} *)
	val open_proto_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_proto_tree_db} *)
	val open_proto_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_stash_db} *)
	val open_stash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?bnum:Openparam.t_long ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_cache_hash_db} *)
	val open_cache_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_cache_tree_db} *)
	val open_cache_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_file_hash_db} *)
	val open_file_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_file_tree_db} *)
	val open_file_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_dir_hash_db} *)
	val open_dir_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_dir_tree_db} *)
	val open_dir_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.open_text_db} *)
	val open_text_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 string ->
	 string ->
	 t_open_mode list ->
	bool

(** {b See} {! Kyotocaml.KCDb.Ret.close_db} *)
	val close_db: t -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.set} *)
	val set: t -> string -> KCString.t -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.set_string} *)
	val set_string: t -> string -> string -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.add} *)
	val add: t -> string -> KCString.t -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.add_string} *)
	val add_string: t -> string -> string -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.replace} *)
	val replace: t -> string -> KCString.t -> bool
	
(** {b See} {! Kyotocaml.KCDb.Ret.replace_with_string} *)
	val replace_with_string: t -> string -> string -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.append} *)
	val append: t -> string -> KCString.t -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.append_string} *)
	val append_string: t -> string -> string -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.remove} *)
	val remove: t -> string -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.get} *)
	val get: t -> string -> KCString.t option

(** {b See} {! Kyotocaml.KCDb.Ret.get_as_string} *)
	val get_as_string: t -> string -> string option

(** {b See} {! Kyotocaml.KCDb.Ret.sync} *)
	val sync: t -> bool -> 'a t_fileproc -> 'a -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.clear} *)
	val clear: t -> bool

(** [inner_db_operation db fun] *)
	val inner_db_operation: t -> (KCDb.t -> 'a) -> 'a option

(** {b See} {! Kyotocaml.KCDb.Ret.setsub} *)
	val setsub: t -> string -> KCString.t -> int -> int -> bool
	
(** {b See} {! Kyotocaml.KCDb.Ret.settail} *)
	val settail: t -> string -> KCString.t -> int -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.addsub} *)
	val addsub: t -> string -> KCString.t -> int -> int -> bool
	
(** {b See} {! Kyotocaml.KCDb.Ret.addtail} *)
	val addtail: t -> string -> KCString.t -> int -> bool

(** {b See} {! Kyotocaml.KCDb.Ret.replace_with_sub} *)
	val replace_with_sub: t -> string -> KCString.t -> int -> int -> bool
	
(** {b See} {! Kyotocaml.KCDb.Ret.replace_with_tail} *)
	val replace_with_tail: t -> string -> KCString.t -> int -> bool
	
(** {b See} {! Kyotocaml.KCDb.Ret.appendsub} *)
	val appendsub: t -> string -> KCString.t -> int -> int -> bool
	
(** {b See} {! Kyotocaml.KCDb.Ret.appendtail} *)
	val appendtail: t -> string -> KCString.t -> int -> bool

end (* Ret *)

module Exc : sig

(** {b See} {! Kyotocaml.KCDb.Exc.make} *)
	val make: unit -> t

(** {b See} {! Kyotocaml.KCDb.Exc.open_db} *)
	val open_db: t -> string -> t_open_mode list -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_proto_hash_db} *)
	val open_proto_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_proto_tree_db} *)
	val open_proto_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_stash_db} *)
	val open_stash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?bnum:Openparam.t_long ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_cache_hash_db} *)
	val open_cache_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_cache_tree_db} *)
	val open_cache_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
 	?capcnt:Openparam.t_long ->
 	?capsiz:Openparam.t_long ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_file_hash_db} *)
	val open_file_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_file_tree_db} *)
	val open_file_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?apow:Openparam.t_int ->
	?fpow:Openparam.t_int ->
	?opts:Openparam.t_opts ->
	?bnum:Openparam.t_long ->
	?msiz:Openparam.t_long ->
	?dfunit:Openparam.t_long ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_dir_hash_db} *)
	val open_dir_hash_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_dir_tree_db} *)
	val open_dir_tree_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	?opts:Openparam.t_opts ->
	?zcmp:Openparam.t_zcmp ->
	?zkey:Openparam.t_long ->
	?psiz:Openparam.t_long ->
	?rcomp:Openparam.t_rcomp ->
	?pccap:Openparam.t_long ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.open_text_db} *)
	val open_text_db:
	t ->
	?log:Openparam.t_log -> 
	?logkinds:Openparam.t_logkinds -> 
	?logpx:Openparam.t_logpx ->
	 string ->
	 string ->
	 t_open_mode list ->
	unit

(** {b See} {! Kyotocaml.KCDb.Exc.close_db} *)
	val close_db: t -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.set} *)
	val set: t -> string -> KCString.t -> unit
	
(** {b See} {! Kyotocaml.KCDb.Exc.set_string} *)
	val set_string: t -> string -> string -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.add} *)
	val add: t -> string -> KCString.t -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.add_string} *)
	val add_string: t -> string -> string -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.replace} *)
	val replace: t -> string -> KCString.t -> unit
	
(** {b See} {! Kyotocaml.KCDb.Exc.replace_with_string} *)
	val replace_with_string: t -> string -> string -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.append} *)
	val append: t -> string -> KCString.t -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.append_string} *)
	val append_string: t -> string -> string -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.remove} *)
	val remove: t -> string -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.get} *)
	val get: t -> string -> KCString.t
	
(** {b See} {! Kyotocaml.KCDb.Exc.get_as_string} *)
	val get_as_string: t -> string -> string
	
(** {b See} {! Kyotocaml.KCDb.Exc.sync} *)
	val sync: t -> bool -> 'a t_fileproc -> 'a -> unit
	
(** {b See} {! Kyotocaml.KCDb.Exc.clear} *)
	val clear: t -> unit
	
(** [inner_db_operation db fun] *)
	val inner_db_operation: t -> (KCDb.t -> 'a) -> 'a

(** {b See} {! Kyotocaml.KCDb.Exc.setsub} *)
	val setsub: t -> string -> KCString.t -> int -> int -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.settail} *)
	val settail: t -> string -> KCString.t -> int -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.addsub} *)
	val addsub: t -> string -> KCString.t -> int -> int -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.addtail} *)
	val addtail: t -> string -> KCString.t -> int -> unit
	
(** {b See} {! Kyotocaml.KCDb.Exc.replace_with_sub} *)
	val replace_with_sub: t -> string -> KCString.t -> int -> int -> unit

(** {b See} {! Kyotocaml.KCDb.Exc.replace_with_tail} *)
	val replace_with_tail: t -> string -> KCString.t -> int -> unit
	
(** {b See} {! Kyotocaml.KCDb.Exc.appendsub} *)
	val appendsub: t -> string -> KCString.t -> int -> int -> unit
	
(** {b See} {! Kyotocaml.KCDb.Exc.appendtail} *)
	val appendtail: t -> string -> KCString.t -> int -> unit

end (* Ret *)

end (* KCIdx *)


