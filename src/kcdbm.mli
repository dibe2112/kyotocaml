(*
 * kcdbm - a replacement for module CamlDBM, using kyoto cabinet 
 *                  as database backend; based on the kyotocaml binding
 *
 * Copyright (C) 2013-  
 *      D. Beutner  
 *      dbe dot mailbox at gmail dot com
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
 
(**  Replacement for the (deprecated) Dbm module from ocaml standard lib *)
 
(** Kcdbm maps the interface of the Dbm module (now: CamlDBM) to 
Kyoto Cabinet library functions.
{b Note} that only the interface is identical, but the behavior of the database 
may differ in various details *)
 
(** Module Hash: Data is stored in a hash database   *)
module Hash : sig

type t
(** The type of file descriptors opened on KC databases. *)

type open_flag = 
    Dbm_rdonly
  | Dbm_wronly
  | Dbm_rdwr
  | Dbm_create
  | Dbm_trunc | Dbm_autosync 
(** Flags for opening a database (see {!opendbm}). 
Dbm_trunc and Dbm_autosync are special KCDbm options not available in NDbm *)


exception Dbm_error of string
(** Raised by the following functions when an error is encountered. *)

val opendbm : string -> open_flag list -> int -> t
(** Open a descriptor on an KC database. The first argument is
   the name of the database (without the suffix).
   The second argument is a list of flags: [Dbm_rdonly] opens
   the database for reading only, [Dbm_wronly] for writing,
   [Dbm_rdwr] for reading and writing; [Dbm_create] causes the
   database to be created if it does not already exist.
   The third argument is the permissions to give to the database
   files, if the database is created. *)

val close : t -> unit
(** Close the given descriptor. *)

val find : t -> string -> string
(** [find db key] returns the data associated with the given
   [key] in the database opened for the descriptor [db].
   Raise [Not_found] if the [key] has no associated data. *)

val add : t -> string -> string -> unit
(** [add db key data] inserts the pair ([key], [data]) in
   the database [db]. If the database already contains data
   associated with [key], raise [Dbm_error "Entry already exists"]. *)

val replace : t -> string -> string -> unit
(** [replace db key data] inserts the pair ([key], [data]) in
   the database [db]. If the database already contains data
   associated with [key], that data is discarded and silently
   replaced by the new [data]. *)

val remove : t -> string -> unit
(** [remove db key data] removes the data associated with [key]
   in [db]. If [key] has no associated data, raise
   [Dbm_error "dbm_delete"]. *)

val firstkey : t -> string
(** See {!nextkey}.*)

val nextkey : t -> string
(** Enumerate all keys in the given database, in an unspecified order.
   [firstkey db] returns the first key, and repeated calls
   to [nextkey db] return the remaining keys. [Not_found] is raised
   when all keys have been enumerated. *)

val iter : (string -> string -> 'a) -> t -> unit
(** [iter f db] applies [f] to each ([key], [data]) pair in
   the database [db]. [f] receives [key] as first argument
   and [data] as second argument. *)

val begintran : t -> bool -> bool
(** [begintran db] starts a transaction on the database [db]. *)

val begintrantry : t -> bool -> bool
(** [begintrantry db] trys to start a transaction on the database [db]. *)

val commit : t -> bool
(** [commit db] ends a transaction on the database [db] and confirms all changes
 of data during transaction. *)

val rollback : t -> bool
(** [rollback db] ends a transaction on the database [db] and abandons all changes
 of data during transaction. *)

val dump: t -> string -> bool
(** [dump db path] writes all datasets of database db to a file *) 

val load: string -> t -> bool
(** [load path db] transfers data from files created with [dump] back to database *) 

val sync: t -> bool -> bool
(** [sync db false] synchronizes the database with the filesysetem;
[sync db false] synchronizes the database with the physical device *) 

end (* Hash *)

(** Module Tree: data is stored in a tree database *)
module Tree : sig

type t
(** The type of file descriptors opened on KC databases. *)

type open_flag = 
    Dbm_rdonly
  | Dbm_wronly
  | Dbm_rdwr
  | Dbm_create
  | Dbm_trunc | Dbm_autosync 
(** Flags for opening a database (see {!opendbm}). 
Dbm_trunc and Dbm_autosync are special KCDbm options not available in NDbm *)

exception Dbm_error of string
(** Raised by the following functions when an error is encountered. *)

val opendbm : string -> open_flag list -> int -> t
(** Open a descriptor on an KC database. The first argument is
   the name of the database (without the suffix).
   The second argument is a list of flags: [Dbm_rdonly] opens
   the database for reading only, [Dbm_wronly] for writing,
   [Dbm_rdwr] for reading and writing; [Dbm_create] causes the
   database to be created if it does not already exist.
   The third argument is the permissions to give to the database
   files, if the database is created. *)

val close : t -> unit
(** Close the given descriptor. *)

val find : t -> string -> string
(** [find db key] returns the data associated with the given
   [key] in the database opened for the descriptor [db].
   Raise [Not_found] if the [key] has no associated data. *)

val add : t -> string -> string -> unit
(** [add db key data] inserts the pair ([key], [data]) in
   the database [db]. If the database already contains data
   associated with [key], raise [Dbm_error "Entry already exists"]. *)

val replace : t -> string -> string -> unit
(** [replace db key data] inserts the pair ([key], [data]) in
   the database [db]. If the database already contains data
   associated with [key], that data is discarded and silently
   replaced by the new [data]. *)

val remove : t -> string -> unit
(** [remove db key data] removes the data associated with [key]
   in [db]. If [key] has no associated data, raise
   [Dbm_error "dbm_delete"]. *)

val firstkey : t -> string
(** See {!nextkey}.*)

val nextkey : t -> string
(** Enumerate all keys in the given database, in an unspecified order.
   [firstkey db] returns the first key, and repeated calls
   to [nextkey db] return the remaining keys. [Not_found] is raised
   when all keys have been enumerated. *)

val iter : (string -> string -> 'a) -> t -> unit
(** [iter f db] applies [f] to each ([key], [data]) pair in
   the database [db]. [f] receives [key] as first argument
   and [data] as second argument. *)

(** {5 Extension} *)
val begintran : t -> bool -> bool
(** [begintran db] starts a transaction on the database [db]. *)

val begintrantry : t -> bool -> bool
(** [begintrantry db] trys to start a transaction on the database [db]. *)

val commit : t -> bool
(** [commit db] ends a transaction on the database [db] and confirms all changes
 of data during transaction. *)

val rollback : t -> bool
(** [rollback db] ends a transaction on the database [db] and abandons all changes
 of data during transaction. *)

val dump: t -> string -> bool
(** [dump db path] writes all datasets of database db to a file *) 

val load: string -> t -> bool
(** [load path db] transfers data from files created with [dump] back to database *) 

val sync: t -> bool -> bool
(** [sync db false] synchronizes the database with the filesysetem;
[sync db false] synchronizes the database with the physical device *) 

end (* Tree *)
