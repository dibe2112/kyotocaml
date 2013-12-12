/*
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
 */


#include <assert.h>
#include <string.h> 
#include <caml/memory.h> 
#include <caml/alloc.h> 
#include <caml/callback.h> 
#include <caml/custom.h> 
#include <caml/fail.h> 
#include <caml/signals.h>

#include "ocinterf-util.h"
#include "kcstring.h" 
#include "kcdbcallback.h"
#include <kclangc.h>
#include "kccur.h"

/*********************************************************************/

#define KCDb_val(V) ((KCDb *) Data_custom_val(V))
#define KCDb_val_p(V) ((KCDb_val(V))->p)

#define EXC_KEYNOTFOUND(MSG) raise_keynotfound(MSG)

#undef _EXC
#define _EXC(A) "kcdb." A

#ifdef FINMGS_DB
#define FINALMSG(S,A) printf(S "{%#lx}\n", (unsigned long)(A)); fflush(stdout);
#else
#define FINALMSG(S,A)
#endif

#undef KCDB_IDENTIFIER
#define KCDB_IDENTIFIER "kyotOCAml.kcdb.1.0"
/*********************************************************************/
CAMLprim value 
kc_version(value arg_unit) {
	CAMLparam1(arg_unit);
	CAMLreturn(caml_copy_string(KCVERSION));
}

void 
raise_keynotfound(const char* msg) {
	static value * exc = NULL;
	if(!exc) {
		exc = caml_named_value("kyotOCAml_Exc_KeyNotFound");
		assert(exc);
	}
	if(msg) caml_raise_with_string(*exc, msg);
	return;
}
/*********************************************************************/
typedef struct _KCDb {
   KCDB * p;
} KCDb;

static size_t
sizeofKCDb = sizeof(KCDb);

static void 
_custom_finalize_kcdb(value vobj) {
	KCDb * pdb =  KCDb_val(vobj);
	assert(pdb);
	assert(pdb->p);
	kcdbclose(pdb->p);  
	FINALMSG("##KCDB object to delete", pdb->p);
	kcdbdel(pdb->p);
	FINALMSG("##KCDB object was deleted", 0);
}

static struct custom_operations 
_custops_kcdb = {
  identifier: KCDB_IDENTIFIER,
  finalize: _custom_finalize_kcdb,
  compare: custom_compare_default,
  hash: custom_hash_default,
  serialize: custom_serialize_default,
  deserialize: custom_deserialize_default 
};

static struct custom_operations 
_custops_kcdb_nofinal = {
  identifier: KCDB_IDENTIFIER,
  finalize: custom_finalize_default,
  compare: custom_compare_default,
  hash: custom_hash_default,
  serialize: custom_serialize_default,
  deserialize: custom_deserialize_default 
};

/*********************************************************************/
CAMLprim value 
copy_kcdb(const KCDb * pdb) {
   CAMLparam0 ();
   CAMLlocal1 (vret);

   vret = caml_alloc_custom( &_custops_kcdb, sizeofKCDb, 0, 1);
   memcpy( (void*)Data_custom_val(vret), (const void*)pdb, sizeofKCDb);
   CAMLreturn(vret);
}

CAMLprim value 
value_of_KCDB_nofinal(const KCDB * p) {
   CAMLparam0 ();
   CAMLlocal1 (vret);

	const KCDb kcdb =  { (KCDB*)p };
   vret = caml_alloc_custom( &_custops_kcdb_nofinal, sizeofKCDb, 0, 1);
   memcpy( (void*)Data_custom_val(vret), (const void*)(&kcdb), sizeofKCDb);
   CAMLreturn(vret);
}

CAMLprim value
caml_kcdb_make(value arg_unit) {
   CAMLparam1(arg_unit);
	
   RXA( KCDb db = { kcdbnew() } );
 	RET_OPT((db.p), copy_kcdb(&db));
}

CAMLprim value
caml_kcdb_make_exc(value arg_unit) {
   CAMLparam1(arg_unit);

   RXA( KCDb db =  { kcdbnew() } );
   if(!db.p) caml_raise_out_of_memory();
   CAMLreturn(copy_kcdb(&db));
}

uint32_t 
get_mode(value arg) {

   static const uint32_t mode_table[] = { 
		KCOWRITER, KCOREADER, 
		KCOCREATE, KCOTRUNCATE, KCOAUTOTRAN, KCOAUTOSYNC, 
		KCONOLOCK, KCOTRYLOCK, KCONOREPAIR
	}; 
	uint32_t mode;
	if(arg == Val_emptylist) {
		mode = KCOWRITER | KCOCREATE; 
	}
	else {
	   mode = mode ^ mode;
   	do {
 			value head = Field(arg, 0);
 			int64_t n = Long_val(head);
 			assert(n < (sizeof mode_table)/(sizeof mode_table[0]));
        	mode |= mode_table[n];
        	arg = Field(arg, 1);
   	} while (arg != Val_emptylist);
	}
	return mode;
}

CAMLprim value
caml_kcdb_open(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const uint32_t mode = get_mode(arg3);
	KCDb * pdb = KCDb_val(arg1);
   RXA( const int32_t r = kcdbopen(pdb->p, String_val(arg2), mode) );
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_open_exc(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const uint32_t mode = get_mode(arg3);
   RXA( const int32_t r = kcdbopen(KCDb_val_p(arg1), String_val(arg2), mode) );
   if(!r) caml_failwith( _EXC("open") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_close(value arg1) {
	CAMLparam1(arg1);

   RXA( const int32_t r = kcdbclose(KCDb_val_p(arg1)) );
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_close_exc(value arg1) {
	CAMLparam1(arg1);

   RXA( const int32_t r = kcdbclose(KCDb_val_p(arg1)) );
   if(!r) caml_failwith( _EXC("close") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_ecode(value arg1) {
	CAMLparam1(arg1);
   RXA( const int32_t r = kcdbecode(KCDb_val_p(arg1)) );
   RET_INT(r);
}

CAMLprim value
caml_kcdb_emsg(value arg1) {
	CAMLparam1(arg1);
   RXA( const char * r = kcdbemsg(KCDb_val_p(arg1)) );
   CAMLreturn( COPY_STRING_0(r) );
}


CAMLprim value
caml_kcdb_accept_native(value arg1, value arg2, value arg3, 
	value arg4, value arg5, value arg6) {
	CAMLparam5(arg1,arg2,arg3,arg4, arg5);
	CAMLxparam1(arg6);
	
	CBSTRCT cbs = { &arg3, &arg4, &arg5 };
	RXA(
	const int32_t r =
		kcdbaccept(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
		 	visitfull_func,
			visitempty_func,
			(void*)&cbs,
			Bool_val(arg6)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_accept_exc_native(value arg1, value arg2, value arg3, 
	value arg4, value arg5, value arg6) {
	CAMLparam5(arg1,arg2,arg3,arg4, arg5);
	CAMLxparam1(arg6);
	
	CBSTRCT cbs = { &arg3, &arg4, &arg5 };
	RXA(
	const int32_t r =
		kcdbaccept(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
		 	visitfull_func,
			visitempty_func,
			(void*)&cbs,
			Bool_val(arg6)
		)
	);
   if(!r) caml_failwith( _EXC("accept") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_accept_bytecode(value * argv, int argn) {

	assert(6 == argn);
	return
	caml_kcdb_accept_native(argv[0], argv[1], 
		argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value
caml_kcdb_accept_exc_bytecode(value * argv, int argn) {

	assert(6 == argn);
	return
	caml_kcdb_accept_exc_native(argv[0], argv[1], 
		argv[2], argv[3], argv[4], argv[5]);
}


const char*
visitfull_func_test(const char *kbuf, size_t ksiz, 
	const char *vbuf, size_t vsiz, size_t *sp, void *opq) {
	
	CAMLparam0();
	static unsigned long count = 0ul;

	CAMLlocal2(vkey, vval);

	vkey = copy_buffer_to_kcstring_nofree(kbuf, ksiz);
	vval = copy_buffer_to_kcstring_nofree(vbuf, vsiz); 

	if(!((count++) % 1000) && vkey && vval) { printf("_"); fflush(stdout); }
	CAMLreturnT(const char*, KCVISNOP);
}


CAMLprim value
caml_kcdb_iterate(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);
	
	CBSTRCT cbs = { &arg2, NULL, &arg3 };
	RXA(
	const int32_t r =
		kcdbiterate(
			KCDb_val_p(arg1),
		 	visitfull_func,
			(void*)&cbs,
			Bool_val(arg4)
		);
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_iterate_string(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);
	
	CBSTRCT cbs = { &arg2, NULL, &arg3 };
	RXA(
	const int32_t r =
		kcdbiterate(
			KCDb_val_p(arg1),
		 	visitfull_string_func,
			(void*)&cbs,
			Bool_val(arg4)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_iterate_exc(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);
	
	CBSTRCT cbs = { &arg2, NULL, &arg3 };
	RXA(
	const int32_t r =
		kcdbiterate(
			KCDb_val_p(arg1),
		 	visitfull_func,
			(void*)&cbs,
			Bool_val(arg4)
		)
	);
   if(!r) caml_failwith( _EXC("iterate") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_iterate_string_exc(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);
	
	CBSTRCT cbs = { &arg2, NULL, &arg3 };
	RXA(
	const int32_t r =
		kcdbiterate(
			KCDb_val_p(arg1),
		 	visitfull_string_func,
			(void*)&cbs,
			Bool_val(arg4)
		)
	);
   if(!r) caml_failwith( _EXC("iterate_string") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_set(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_set_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   if(!r) caml_failwith( _EXC("set") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_set_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_set_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   if(!r) caml_failwith( _EXC("set_string") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_add(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r = 
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_add_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r = 
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   if(!r) caml_failwith( _EXC("add") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_add_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_add_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   if(!r) caml_failwith( _EXC("add_string") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_replace(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r =
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2), 
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_replace_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r =
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2), 
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   if(!r) caml_failwith( _EXC("replace") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_replace_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_replace_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   if(!r) caml_failwith( _EXC("replace_with_string") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_append(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r =
		kcdbappend(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_append_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA( 
   const int32_t r =
		kcdbappend(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		)
	);
   if(!r) caml_failwith( _EXC("append") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_append_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbappend(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_append_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbappend(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   if(!r) caml_failwith( _EXC("append_string") );
	RET_UNIT;
}


CAMLprim value
caml_kcdb_incrint(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3, arg4);

	RXA( 
   const int64_t r =
		kcdbincrint(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			Int64_val(arg3),
			Int64_val(arg4)
		)
	);
   RET_LONG(r);
}

CAMLprim value
caml_kcdb_incrint_exc(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3, arg4);

	RXA( 
   const int64_t r =
		kcdbincrint(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			Int64_val(arg3),
			Int64_val(arg4)
		)
	);
   if(INT64_MAX == r) caml_failwith( _EXC("incrint") );
   RET_LONG(r);
}

CAMLprim value
caml_kcdb_incrdouble(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3, arg4);

	RXA( 
   const double r =
		kcdbincrdouble(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			Double_val(arg3),
			Double_val(arg4)
		)
	);
   RET_FLOAT(r);
}

CAMLprim value
caml_kcdb_incrdouble_exc(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3, arg4);

	RXA( 
   const double r =
		kcdbincrdouble(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			Double_val(arg3),
			Double_val(arg4)
		)
	);
   if(kcchknan(r)) caml_failwith( _EXC("incrdouble") );
   RET_FLOAT(r);
}

CAMLprim value
caml_kcdb_cas(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1, arg2, arg3, arg4);

   KCString * pkcstr1 = KCString_val(arg3);
   KCString * pkcstr2 = KCString_val(arg4);
	RXA( 
   const int32_t r =
		kcdbcas(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr1->buf,
			pkcstr1->size,
			pkcstr2->buf,
			pkcstr2->size
		)
	);
	RET_BOOL(r);	
}

CAMLprim value
caml_kcdb_cas_string(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1, arg2, arg3, arg4);

   KCString * pkcstr1 = KCString_val(arg4);
	RXA( 
   const int32_t r =
		kcdbcas(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3),
			pkcstr1->buf,
			pkcstr1->size
		)
	);
	RET_BOOL(r);	
}

CAMLprim value
caml_kcdb_cas_stringstring(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1, arg2, arg3, arg4);

	RXA( 
   const int32_t r =
		kcdbcas(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3),
			String_val(arg4),
			caml_string_length(arg4)
		)
	);
	RET_BOOL(r);	
}

CAMLprim value
caml_kcdb_remove(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA( 
   const int32_t r =
		kcdbremove(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
 	RET_BOOL(r);
}

CAMLprim value
caml_kcdb_remove_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA( 
   const int32_t r =
		kcdbremove(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
   if(!r) EXC_KEYNOTFOUND( _EXC("remove") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_get(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA( 
	KCString kcstr;
   kcstr.buf = 
		kcdbget(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&kcstr.size
		)
	);
 	RET_OPT((kcstr.buf), copy_kcstring(&kcstr));
}

CAMLprim value
caml_kcdb_get_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA( 
	KCString kcstr;
   kcstr.buf = 
		kcdbget(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&kcstr.size
		)
	);
   if(!kcstr.buf) EXC_KEYNOTFOUND( _EXC("get") );
   CAMLreturn(copy_kcstring(&kcstr));
}

CAMLprim value
caml_kcdb_get_as_string(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size = 0;
	RXA( 
   const char * r = 
		kcdbget(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&size
		)
	);
   if(!r) RET_NONE;
	vret = copy_buffer_to_string(r, size);
	kcfree((char*)r); 
 	RET_SOME(vret);
}

CAMLprim value
caml_kcdb_get_as_string_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size = 0;
	RXA( 
   const char * r = 
		kcdbget(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&size
		)
	);
   if(!r) EXC_KEYNOTFOUND( _EXC("get_as_string") );
	vret = copy_buffer_to_string(r, size);
	kcfree((char*)r); 
   CAMLreturn( vret );
}

CAMLprim value
caml_kcdb_getbuf(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbgetbuf(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
	RET_INT(r);
}

CAMLprim value
caml_kcdb_getbuf_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA( 
   const int32_t r =
		kcdbgetbuf(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   if(r < 0) EXC_KEYNOTFOUND( _EXC("get_buf") );
	RET_INT(r);
}


CAMLprim value
caml_kcdb_check(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbcheck(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
	RET_OPT( r>= 0, Val_int((int)r));
}

CAMLprim value
caml_kcdb_check_exc(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbcheck(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
   if(r  < 0) EXC_KEYNOTFOUND( _EXC("check") );
   RET_INT((int)r);
}


CAMLprim value
caml_kcdb_seize(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA( 
	KCString kcstr;
   kcstr.buf =
		kcdbseize(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&kcstr.size
		)
	);
 	RET_OPT((kcstr.buf), copy_kcstring(&kcstr));
}

CAMLprim value
caml_kcdb_seize_as_string(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size = 0;
	RXA( 
   const char * r =
		kcdbseize(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&size
		)
	);
   if(!r) RET_NONE;
	vret = copy_buffer_to_string(r, size);
	kcfree((char*)r); 
 	RET_SOME(vret);
}

CAMLprim value
caml_kcdb_seize_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA( 
	KCString kcstr;
   kcstr.buf =
		kcdbseize(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&kcstr.size
		)
	);
   if(!kcstr.buf) EXC_KEYNOTFOUND( _EXC("seize") );
   CAMLreturn(copy_kcstring(&kcstr));
}

CAMLprim value
caml_kcdb_seize_as_string_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size = 0;
	RXA( 
   const char * r =
		kcdbseize(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&size
		)
	);
   if(!r) EXC_KEYNOTFOUND( _EXC("seize_as_string") );
	vret = copy_buffer_to_string(r, size);
	kcfree((char*)r); 
   CAMLreturn( vret );
}

CAMLprim value
caml_kcdb_sync(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	int32_t r = 0;

   if (Is_long(arg3)) {
		RXA( 
			r =
			kcdbsync(
				KCDb_val_p(arg1),
				Bool_val(arg2),
				NULL,
				NULL
			)
		);
	}
	else if (Is_block(arg3)) {
		assert(Tag_val(arg3) == 0);
		value vf = Field(arg3,0);
		CBSTRCT cbs = { &vf, NULL, &arg4 };
		RXA( 
			r =
			kcdbsync(
				KCDb_val_p(arg1),
				Bool_val(arg2),
				fileproc_func,
				(void*)&cbs
			)
		);
	}
	else assert(0); 
 
	RET_BOOL(r);
}

CAMLprim value
caml_kcdb_sync_exc(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	int32_t r = 0;

   if (Is_long(arg3)) {
		RXA( 
			r =
			kcdbsync(
				KCDb_val_p(arg1),
				Bool_val(arg2),
				NULL,
				NULL
			)
		);
	}
	else if (Is_block(arg3)) {
		assert(Tag_val(arg3) == 0);
		value vf = Field(arg3,0);
		CBSTRCT cbs = { &vf, NULL, &arg4 };
		RXA( 
			r =
			kcdbsync(
				KCDb_val_p(arg1),
				Bool_val(arg2),
				fileproc_func,
				(void*)&cbs
			)
		);
	}
	else assert(0);
 
	if(!r) caml_failwith( _EXC("sync") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_occupy(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	CBSTRCT cbs = { &arg3, NULL, &arg4 };
	RXA( 
	const int32_t r =
		kcdboccupy(
			KCDb_val_p(arg1),
			Bool_val(arg2),
			fileproc_func,
			(void*)&cbs
		)
	);
	RET_BOOL(r);
}

CAMLprim value
caml_kcdb_occupy_exc(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	CBSTRCT cbs = { &arg3, NULL, &arg4 };
	RXA( 
	const int32_t r =
		kcdboccupy(
			KCDb_val_p(arg1),
			Bool_val(arg2),
			fileproc_func,
			(void*)&cbs
		)
	);
	if(!r) caml_failwith( _EXC("sync") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_copy(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbcopy(
			KCDb_val_p(arg1),
			String_val(arg2)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_copy_exc(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbcopy(
			KCDb_val_p(arg1),
			String_val(arg2)
		)
	);
	if(!r) caml_failwith( _EXC("copy") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_begintran(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbbegintran(
			KCDb_val_p(arg1),
			Int_val(arg2)
		)	
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_begintran_exc(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbbegintran(
			KCDb_val_p(arg1),
			Int_val(arg2)
		)	
	);
	if(!r) caml_failwith( _EXC("begintran") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_begintrantry(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbbegintrantry(
			KCDb_val_p(arg1),
			Int_val(arg2)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_begintrantry_exc(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbbegintrantry(
			KCDb_val_p(arg1),
			Int_val(arg2)
		)
	);
	if(!r) caml_failwith( _EXC("begintrantry") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_endtran(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbendtran(
			KCDb_val_p(arg1),
			Int_val(arg2)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_endtran_exc(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA( 
   const int32_t r =
		kcdbendtran(
			KCDb_val_p(arg1),
			Int_val(arg2)
		)
	);
	if(!r) caml_failwith( _EXC("endtran") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_clear(value arg1) {
   CAMLparam1(arg1);

	RXA( const int32_t r = kcdbclear(KCDb_val_p(arg1)) );
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_clear_exc(value arg1) {
   CAMLparam1(arg1);

	RXA( const int32_t r = kcdbclear(KCDb_val_p(arg1)) );
	if(!r) caml_failwith( _EXC("clear") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_dumpsnap(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA(
   const int32_t r =
		kcdbdumpsnap(
			KCDb_val_p(arg1),
			String_val(arg2)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_loadsnap(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

	RXA(
   const int32_t r =
		kcdbloadsnap(
			KCDb_val_p(arg1),
			String_val(arg2)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_count(value arg1) {
   CAMLparam1(arg1);
	RXA( const int64_t sz = kcdbcount(KCDb_val_p(arg1)) );
 	RET_LONG(sz);
}

CAMLprim value
caml_kcdb_size(value arg1) {
   CAMLparam1(arg1);
	RXA( const int64_t sz = kcdbsize(KCDb_val_p(arg1)) );
 	RET_LONG(sz);
}

CAMLprim value
caml_kcdb_path(value arg1) {
   CAMLparam1(arg1);
	CAMLlocal1(vr);
	
	RXA( const char * r = kcdbpath(KCDb_val_p(arg1)) );
	vr = COPY_STRING_0(r);
	kcfree((void*)r);
	CAMLreturn(vr);
}

CAMLprim value
caml_kcdb_status(value arg1) {
	CAMLparam1(arg1);
	CAMLlocal1(vret);

   RXA( const char * r = kcdbstatus(KCDb_val_p(arg1)) );
	vret = r ? Val_some(caml_copy_string(r)) : Val_none;
	if(r) kcfree((void*)r);
 	CAMLreturn(vret);
}

CAMLprim value
caml_kcdb_matchprefix(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1, arg2, arg3);
	CAMLlocal1(vret);
	
	size_t loop;
	const size_t max = (size_t)Int_val(arg3);
	char ** pp = (char**) kcmalloc((max+1) * sizeof(char*));
	if(!pp) caml_raise_out_of_memory();
	RXA(
		const int64_t r = 
		kcdbmatchprefix(
			KCDb_val_p(arg1),
			String_val(arg2),
			pp,
			max
		);
	);
	if(r >= 0) {
		pp[r] = NULL;
		vret = Val_some(caml_copy_string_array((const char**)pp));
		for(loop = 0; loop < r; loop++) kcfree(pp[loop]);
	}
	else vret = Val_none;			
	kcfree(pp);
	CAMLreturn(vret);
}

CAMLprim value
caml_kcdb_matchprefix_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1, arg2, arg3);
	CAMLlocal1(vret);
	
	size_t loop;
	if(Int_val(arg3) < 1) caml_invalid_argument( _EXC("matchprefix") );
	const size_t max = (size_t)Int_val(arg3);
	char ** pp = (char**) kcmalloc((max+1) * sizeof(char*));
	if(!pp) caml_raise_out_of_memory();
	RXA(
		const int64_t r = 
		kcdbmatchprefix(
			KCDb_val_p(arg1),
			String_val(arg2),
			pp,
			max
		);
	);
	if(r < 0) {
		kcfree(pp);
		caml_failwith( _EXC("matchprefix") );
	}				
	pp[r] = NULL;
	vret = caml_copy_string_array((const char**)pp);
	for(loop = 0; loop < r; loop++) kcfree(pp[loop]);
	kcfree(pp);
	CAMLreturn(vret);
}

CAMLprim value
caml_kcdb_matchregex(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1, arg2, arg3);
	CAMLlocal1(vret);
	
	size_t loop;
	const size_t max = (size_t)Int_val(arg3);
	char ** pp = (char**) kcmalloc((max+1) * sizeof(char*));
	if(!pp) caml_raise_out_of_memory();
	RXA(
		const int64_t r = 
		kcdbmatchregex(
			KCDb_val_p(arg1),
			String_val(arg2),
			pp,
			max
		);
	);
	if(r >= 0) {
		pp[r] = NULL;
		vret = Val_some(caml_copy_string_array((const char**)pp));
		for(loop = 0; loop < r; loop++) kcfree(pp[loop]);
	}
	else vret = Val_none;			
	kcfree(pp);
	CAMLreturn(vret);
}

CAMLprim value
caml_kcdb_matchregex_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1, arg2, arg3);
	CAMLlocal1(vret);
	
	size_t loop;
	if(Int_val(arg3) < 1) caml_invalid_argument( _EXC("matchregex") );
	const size_t max = (size_t)Int_val(arg3);
	char ** pp = (char**) kcmalloc((max+1) * sizeof(char*));
	if(!pp) caml_raise_out_of_memory();
	RXA(
		const int64_t r = 
		kcdbmatchregex(
			KCDb_val_p(arg1),
			String_val(arg2),
			pp,
			max
		);
	);
	if(r < 0) {
		kcfree(pp);
		caml_failwith( _EXC("matchregex") );
	}				
	pp[r] = NULL;
	vret = caml_copy_string_array((const char**)pp);
	for(loop = 0; loop < r; loop++) kcfree(pp[loop]);
	kcfree(pp);
	CAMLreturn(vret);
}

CAMLprim value
caml_kcdb_matchsimilar(value arg1, value arg2, value arg3
	,value arg4, value arg5) {
	CAMLparam5(arg1, arg2, arg3, arg4, arg5);
	CAMLlocal1(vret);
	
	size_t loop;
	const size_t max = (size_t)Int_val(arg5);
	char ** pp = (char**) kcmalloc((max+1) * sizeof(char*));
	if(!pp) caml_raise_out_of_memory();
	const uint32_t dist = (Int_val(arg3) <= 0) ? 0u : ((uint32_t) Int_val(arg3)); 
	RXA(
		const int64_t r = 
		kcdbmatchsimilar(
			KCDb_val_p(arg1),
			String_val(arg2),
			dist,
			Bool_val(arg4),
			pp,
			max
		);
	);
	if(r >= 0) {
		pp[r] = NULL;
		vret = Val_some(caml_copy_string_array((const char**)pp));
		for(loop = 0; loop < r; loop++) kcfree(pp[loop]);
	}
	else vret = Val_none;			
	kcfree(pp);
	CAMLreturn(vret);
}

CAMLprim value
caml_kcdb_matchsimilar_exc(value arg1, value arg2, value arg3
	,value arg4, value arg5) {
	CAMLparam5(arg1, arg2, arg3, arg4, arg5);
	CAMLlocal1(vret);
	
	size_t loop;
	if(Int_val(arg5) < 1) caml_invalid_argument( _EXC("matchsimilar") );
	const size_t max = (size_t)Int_val(arg5);
	char ** pp = (char**) kcmalloc((max+1) * sizeof(char*));
	if(!pp) caml_raise_out_of_memory();
	const uint32_t dist = (Int_val(arg3) <= 0) ? 0u : (uint32_t)Int_val(arg3); 
	RXA(
		const int64_t r = 
		kcdbmatchsimilar(
			KCDb_val_p(arg1),
			String_val(arg2),
			dist,
			Bool_val(arg4),
			pp,
			max
		);
	);
	if(r < 0) {
		kcfree(pp);
		caml_failwith( _EXC("matchsimilar") );
	}				
	pp[r] = NULL;
	vret = caml_copy_string_array((const char**)pp);
	for(loop = 0; loop < r; loop++) kcfree(pp[loop]);
	kcfree(pp);
	CAMLreturn(vret);
}

static int32_t
_caml_kcdb_merge(KCDB * pdb, value arg_arr, int ndx) {

	size_t loop;
	const size_t arr_len = (size_t)Wosize_val(arg_arr);
	KCDB ** pDBsrc = (KCDB **)kcmalloc(arr_len * sizeof(KCDb*));
	if(!pDBsrc) caml_raise_out_of_memory();
	for(loop = 0; loop < arr_len; loop++) {
		pDBsrc[loop] = KCDb_val_p(Field(arg_arr, loop));
	};
	static const uint32_t mode[] = { KCMSET, KCMADD, KCMREPLACE, KCMAPPEND };
	assert(ndx >= 0 && ndx < (sizeof mode)/(sizeof mode[0]));
	RXA(
		const int32_t r = 
		kcdbmerge(
			pdb,
			pDBsrc,
			arr_len,
			mode[ndx]
		);
		kcfree(pDBsrc)
	);
	return r;
}

CAMLprim value
caml_kcdb_merge(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1, arg2, arg3);

	RET_BOOL(_caml_kcdb_merge(KCDb_val_p(arg1), arg2, Val_int(arg3)));
}

CAMLprim value
caml_kcdb_merge_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1, arg2, arg3);

	const int32_t r = _caml_kcdb_merge(KCDb_val_p(arg1), arg2, Val_int(arg3));
	if(!r) caml_failwith( _EXC("merge") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_cursor(value arg1) {
	CAMLparam1(arg1);

	RXA( KCCur cur = { kcdbcursor(KCDb_val_p(arg1)) } );
	RET_OPT((cur.p), mkTpl2(copy_kccur(&cur), arg1));
}

CAMLprim value
caml_kcdb_cursor_exc(value arg1) {
	CAMLparam1(arg1);

	RXA( KCCur cur = { kcdbcursor(KCDb_val_p(arg1)) } );
 	if(!cur.p) caml_failwith( _EXC("cursor") );
	CAMLreturn(mkTpl2(copy_kccur(&cur), arg1));
}

/*===================================================================*/

CAMLprim value
caml_kcdb_setsub(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) CAMLreturn(Val_false);
	RXA( 
	const int len_positive = len >= 0;  
	const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
  RET_BOOL(r);
}

CAMLprim value
caml_kcdb_setsub_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("setsub") ); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) caml_invalid_argument( _EXC("setsub") );
	RXA(
	const int len_positive = len >= 0;  
	const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
	if(!r) caml_failwith( _EXC("setsub") );
  RET_UNIT;
}

CAMLprim value
caml_kcdb_settail(value arg1, value arg2, value arg3, 
	value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
	const size_t len = pkcstr->size - (size_t)n;
	RXA( 
   const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_settail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("settail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
	const int32_t r = 
		kcdbset(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
	if(!r) caml_failwith( _EXC("settail") );
  RET_UNIT;
}

CAMLprim value
caml_kcdb_addsub(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) CAMLreturn(Val_false);
	RXA( 
	const int len_positive = len >= 0;  
	const int32_t r = 
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
  RET_BOOL(r);
}

CAMLprim value
caml_kcdb_addsub_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("addsub") ); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) caml_invalid_argument( _EXC("addsub") );
	RXA(
	const int len_positive = len >= 0;  
	const int32_t r = 
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
	if(!r) caml_failwith( _EXC("addsub") );
	RET_UNIT;
}

CAMLprim value
caml_kcdb_addtail(value arg1, value arg2, value arg3, 
	value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
	const size_t len = pkcstr->size - (size_t)n;
	RXA( 
	const int32_t r = 
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
  RET_BOOL(r);
}

CAMLprim value
caml_kcdb_addtail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("addtail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
	const int32_t r = 
		kcdbadd(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
	if(!r) caml_failwith( _EXC("addtail") );
 RET_UNIT;
}

CAMLprim value
caml_kcdb_replace_with_sub(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) CAMLreturn(Val_false);
	RXA( 
	const int len_positive = len >= 0;  
	const int32_t r = 
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_replace_with_sub_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("replace_with_sub") ); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) caml_invalid_argument( _EXC("replace_with_sub") );
	RXA(
	const int len_positive = len >= 0;  
	const int32_t r = 
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
	if(!r) caml_failwith( _EXC("replace_with_sub") );
 RET_UNIT;
}

CAMLprim value
caml_kcdb_replace_with_tail(value arg1, value arg2, value arg3, 
	value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
	const size_t len = pkcstr->size - (size_t)n;
	RXA( 
	const int32_t r = 
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
 RET_BOOL(r);
}

CAMLprim value
caml_kcdb_replace_with_tail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("replace_with_tail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
	const int32_t r = 
		kcdbreplace(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
   if(!r) caml_failwith( _EXC("replace_with_tail") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_appendsub(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) CAMLreturn(Val_false);
	RXA( 
	const int len_positive = len >= 0;  
   const int32_t r = 
		kcdbappend(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcdb_appendsub_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("appendsub") ); 
	const size_t len = (size_t)Int_val(arg5);
	if(n + len < 0 || n + len >= pkcstr->size) caml_invalid_argument( _EXC("appendsub") );
	RXA(
	const int len_positive = len >= 0;  
   const int32_t r = 
		kcdbappend(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
   if(!r) caml_failwith( _EXC("appendsub") );
   RET_UNIT;
}

CAMLprim value
caml_kcdb_appendtail(value arg1, value arg2, value arg3
,value arg4) {
   CAMLparam4(arg1,arg2,arg3,arg4);

   KCString * pkcstr = KCString_val(arg3);
   const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
   const size_t len = pkcstr->size - (size_t)n;
   RXA( 
        const int32_t r = 
	    kcdbappend(
	        KCDb_val_p(arg1),
                String_val(arg2),
                caml_string_length(arg2),
		pkcstr->buf + n,
                len
	    )
	);
RET_BOOL(r);
}

CAMLprim value
caml_kcdb_appendtail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

	KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
	if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("appendtail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
		const int32_t r = 
		kcdbappend(
			KCDb_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
	if(!r) caml_failwith( _EXC("appendtail") );
RET_UNIT;
}


static int32_t
_setbulk_common(KCDB * pdb, value li, int32_t atomic, void (*convertFunc)(KCSTR*, value)) {

	value head, elem, key, val;

	int32_t r = 0;
	size_t l;
	const size_t rnum = length_of_list(li);
	if(rnum) {
		KCREC * pREC = kcmalloc(rnum * sizeof(KCREC));
		if(!pREC) caml_raise_out_of_memory(); 
		
		head = li;
		for(l = 0; l < rnum; l++) {
			elem = Field(head, 0);
			key = Field(elem, 0);
			val = Field(elem, 1);

			pREC[l].key.buf = String_val(key);
			pREC[l].key.size = caml_string_length(key);
			convertFunc( &pREC[l].value, val);
			head = Field(head, 1);
		}
		RXA( 
			r =
			kcdbsetbulk(
				pdb,
				pREC,
				rnum,
				atomic			
			)		
		);			 
		kcfree(pREC);
	}
	return r;
}

static inline void
_convert_kcstring(KCSTR* p, value v) {
	*p = (*KCString_val(v));
return;
}

static inline void
_convert_mlstring(KCSTR* p, value v) {
	printf("~%s,", String_val(v));
	p->buf =  String_val(v);
	p->size = caml_string_length(v);
return;
}

CAMLprim value
caml_kcdb_setbulk(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal4(head, elem, key, val);
	
	const int32_t r = _setbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), _convert_kcstring);
RET_OPT((r >= 0), Val_int(r));
}

CAMLprim value
caml_kcdb_setbulk_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal4(head, elem, key, val);
	
	const int32_t r = _setbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), _convert_kcstring);
	if(r < 0) caml_failwith( _EXC("setbulk"));
CAMLreturn(Val_int(r));
}

CAMLprim value
caml_kcdb_setbulk_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal4(head, elem, key, val);
	
	const int32_t r = _setbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), _convert_mlstring);
	RET_OPT((r >= 0), Val_int(r));
}

CAMLprim value
caml_kcdb_setbulk_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal4(head, elem, key, val);
	
	const int32_t r = _setbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), _convert_mlstring);
	if(r < 0) caml_failwith( _EXC("setbulk"));
	CAMLreturn(Val_int(r));
}

static int32_t
_removebulk_common(KCDB * pdb, value li, int32_t atomic) {
	
	value head, key;
	int32_t r = 0;
	size_t l;
	const size_t rnum = length_of_list(li);

	if(rnum) {
		KCSTR * pSTR = kcmalloc(rnum * sizeof(KCSTR));
		if(!pSTR) caml_raise_out_of_memory(); 
		
		head = li;
		for(l = 0; l < rnum; l++) {
			key = Field(head, 0);
 
			pSTR[l].buf = String_val(key);
			pSTR[l].size = caml_string_length(key);
			head = Field(head, 1);
		}
		RXA( 
		r =
			kcdbremovebulk(
				pdb,
				pSTR,
				rnum,
				atomic
			)		
		);
		kcfree(pSTR);
	}
	return r;
}

CAMLprim value
caml_kcdb_removebulk(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal4(head, elem, key, val);
	
	const int32_t r = _removebulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3));
	RET_OPT((r >= 0), Val_int(r));
}

CAMLprim value
caml_kcdb_removebulk_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal4(head, elem, key, val);
	
	const int32_t r = _removebulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3));
	if(r < 0) caml_failwith( _EXC("removebulk"));
	CAMLreturn(Val_int(r));
}

int32_t
_getbulk_common(KCDB * pdb, value li_in, int32_t atomic, 
	value * li_out, value (*convertFunc)(const char * p, size_t size)) {

	CAMLparam0();
	CAMLlocal3(head, key, val);
		
	int32_t r = 0, loop;
	size_t loop1;
	const size_t rnum = length_of_list(li_in);

	if(rnum) {
		KCSTR * pSTR = kcmalloc(rnum * sizeof(KCSTR));
		if(!pSTR) caml_raise_out_of_memory(); 
		KCREC * pREC = kcmalloc(rnum * sizeof(KCREC));
		if(!pREC) {
			kcfree(pSTR); 
			caml_raise_out_of_memory();
		} 
		head = li_in;
		for(loop1 = 0; loop1 < rnum; loop1++) {
			key = Field(head, 0);
 
			pSTR[loop1].buf = String_val(key);
			pSTR[loop1].size = caml_string_length(key);
			head = Field(head, 1);
		}
		RXA( 
			r =
			kcdbgetbulk(
				pdb,
				pSTR,
				rnum,
				pREC,
				atomic			
			)		
		);			 
		(*li_out) = Val_emptylist;
		for(loop = r-1; loop >= 0; loop--) {
			const KCREC * pRECaux = pREC + loop;
			assert(loop < r);
			head = caml_alloc(2, 0);
			key = copy_buffer_to_string(pRECaux->key.buf, pRECaux->key.size);
			val = convertFunc(pRECaux->value.buf, pRECaux->value.size);
			Store_field( head, 0, mkTpl2(key, val));
			Store_field( head, 1, (*li_out));

			(*li_out) = head;
		}
		kcfree(pSTR);
		kcfree(pREC);
	}
	CAMLreturnT(int32_t, r);
}

/*
static void
_convert_to_kcstringval(value * v, const KCSTR* p) {
	*v = copy_kcstring(p);
}

static void
_convert_to_mlstringval(value * v, const KCSTR* p) {
	(*v) = caml_alloc_string(p->size);
	memcpy( String_val(*v), p->buf, p->size);
}*/

CAMLprim value
caml_kcdb_getbulk(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal5(head, elem, key, val, li);
	
	const int32_t r = _getbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), &li, copy_buffer_to_kcstring);
	RET_OPT((r >= 0), li);
}

CAMLprim value
caml_kcdb_getbulk_as_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal5(head, elem, key, val, li);
	
	const int32_t r = _getbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), &li, copy_buffer_to_string);
	RET_OPT((r >= 0), li);
}

CAMLprim value
caml_kcdb_getbulk_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal5(head, elem, key, val, li);

	const int32_t r = _getbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), &li, copy_buffer_to_kcstring);
	if(r < 0) caml_failwith( _EXC("getbulk"));
	CAMLreturn(li);
}

CAMLprim value
caml_kcdb_getbulk_as_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);
	CAMLlocal5(head, elem, key, val, li);

	const int32_t r = _getbulk_common(KCDb_val_p(arg1), arg2, Bool_val(arg3), &li, copy_buffer_to_string);
	if(r < 0) caml_failwith( _EXC("getbulk"));
	CAMLreturn(li);
}
