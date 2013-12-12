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
/*********************************************************************/

#define KCIdx_val(V) ((KCIdx *) Data_custom_val(V))
#define KCIdx_val_p(V) ((KCIdx_val(V))->p)

#define EXC_KEYNOTFOUND(MSG) raise_keynotfound(MSG)

#undef _EXC
#define _EXC(A) "kcidx." A

#undef KCIDX_IDENTIFIER
#define KCIDX_IDENTIFIER "kyotOCAml.kcidx.1.0"

extern void raise_keynotfound(const char*);  // => kcdb.c
/*********************************************************************/
typedef struct _KCIdx {
   KCIDX * p;
} KCIdx;

static size_t
sizeofKCIdx = sizeof(KCIdx);

static void 
_custom_finalize_kcidx(value vobj) {
	KCIdx * pidx =  KCIdx_val(vobj);
	assert(pidx);
	assert(pidx->p);
	kcidxclose(pidx->p);  
	kcidxdel(pidx->p);  
	assert( 0 < printf("KCIDX object was deleted\n") );
}

static struct custom_operations 
_custops_kcidx = {
  identifier: KCIDX_IDENTIFIER,
  finalize: _custom_finalize_kcidx,
  compare: custom_compare_default,
  hash: custom_hash_default,
  serialize: custom_serialize_default,
  deserialize: custom_deserialize_default 
};

/*********************************************************************/
CAMLprim value 
copy_kcidx(const KCIdx * pidx) {
   CAMLparam0 ();
   CAMLlocal1 (vret);

   vret = caml_alloc_custom( &_custops_kcidx, sizeofKCIdx, 0, 1);
   memcpy( (void*)Data_custom_val(vret), (const void*)pidx, sizeofKCIdx);
   CAMLreturn(vret);
}

CAMLprim value
caml_kcidx_make(value arg_unit) {
   CAMLparam1(arg_unit);
	
   RXA( KCIdx idx = { kcidxnew() } );
 	RET_OPT((idx.p), copy_kcidx(&idx));
}

CAMLprim value
caml_kcidx_make_exc(value arg_unit) {
   CAMLparam1(arg_unit);

   RXA( KCIdx idx = { kcidxnew() } );
   if(!idx.p) caml_raise_out_of_memory();
   CAMLreturn(copy_kcidx(&idx));
}

extern uint32_t
get_mode(value);  /* -> kcdb.c */

CAMLprim value
caml_kcidx_open(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const uint32_t mode = get_mode(arg3);
  	RXA( const  int32_t r = kcidxopen(KCIdx_val_p(arg1), String_val(arg2), mode) );
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_open_exc(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const uint32_t mode = get_mode(arg3);
  	RXA( const  int32_t r = kcidxopen(KCIdx_val_p(arg1), String_val(arg2), mode) );
   if(!r) caml_failwith( _EXC("open") );
	RET_UNIT;
}

CAMLprim value
caml_kcidx_close(value arg1) {
	CAMLparam1(arg1);

	KCIdx * pidx = KCIdx_val(arg1);
   RXA( const int32_t r = kcidxclose(pidx->p) );
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_close_exc(value arg1) {
	CAMLparam1(arg1);

	KCIdx * pidx = KCIdx_val(arg1);
   RXA( const int32_t r = kcidxclose(pidx->p) );
	if(!r) caml_failwith( _EXC("close") );
   RET_UNIT;
}

CAMLprim value
caml_kcidx_ecode(value arg1) {
	CAMLparam1(arg1);
   RXA( const int32_t r = kcidxecode(KCIdx_val_p(arg1)) );
   RET_INT(r);
}

CAMLprim value
caml_kcidx_emsg(value arg1) {
	CAMLparam1(arg1);
   RXA( const char * r = kcidxemsg(KCIdx_val_p(arg1)) );
   CAMLreturn( COPY_STRING_0(r) );
}

CAMLprim value
caml_kcidx_set(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r = 
		kcidxset(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf, pkcstr->size
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_set_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r = 
		kcidxset(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf, pkcstr->size
		)
	);
   if(!r) caml_failwith( _EXC("set") );
	RET_UNIT;
}

CAMLprim value
caml_kcidx_set_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r =
		kcidxset(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_set_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r =
		kcidxset(
			KCIdx_val_p(arg1),
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
caml_kcidx_add(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r =
		kcidxadd(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		);
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_add_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r =
		kcidxadd(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		);
	);
   if(!r) caml_failwith( _EXC("add") );
	RET_UNIT;
}

CAMLprim value
caml_kcidx_add_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r = 
		kcidxadd(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		);
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_add_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r = 
		kcidxadd(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		);
	);
   if(!r) caml_failwith( _EXC("add_string") );
	RET_UNIT;
}

CAMLprim value
caml_kcidx_replace(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r =
		kcidxreplace(
			KCIdx_val_p(arg1),
			String_val(arg2), 
			caml_string_length(arg2),
			pkcstr->buf, pkcstr->size
		);
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_replace_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r =
		kcidxreplace(
			KCIdx_val_p(arg1),
			String_val(arg2), 
			caml_string_length(arg2),
			pkcstr->buf, pkcstr->size
		);
	);
   if(!r) caml_failwith( _EXC("replace") );
	RET_UNIT;
}

CAMLprim value
caml_kcidx_replace_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r = 
		kcidxreplace(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		);
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_replace_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r = 
		kcidxreplace(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		);
	);
   if(!r) caml_failwith( _EXC("replace_string") );
	RET_UNIT;
}

CAMLprim value
caml_kcidx_append(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r = 
		kcidxappend(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		);
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_append_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg3);
	RXA(
   const int32_t r = 
		kcidxappend(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf,
			pkcstr->size
		);
	);
   if(!r) caml_failwith( _EXC("append") );
	RET_UNIT;
}

CAMLprim value
caml_kcidx_append_string(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r =
		kcidxappend(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2), 
			String_val(arg3),
			caml_string_length(arg3)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_append_string_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

	RXA(
   const int32_t r =
		kcidxappend(
			KCIdx_val_p(arg1),
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
caml_kcidx_remove(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA(
   const int32_t r = 
		kcidxremove(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
 	RET_BOOL(r);
}

CAMLprim value
caml_kcidx_remove_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	RXA(
   const int32_t r =
		kcidxremove(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
   if(!r) EXC_KEYNOTFOUND( _EXC("remove") );
   RET_UNIT;
}

CAMLprim value
caml_kcidx_remove_string(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);

	RXA(
   const int32_t r = 
		kcidxremove(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
 	RET_BOOL(r);
}

CAMLprim value
caml_kcidx_remove_string_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);

	RXA(
   const int32_t r =
		kcidxremove(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
   if(!r) EXC_KEYNOTFOUND( _EXC("remove_string") );
   RET_UNIT;
}

CAMLprim value
caml_kcidx_get(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	KCString kcstr;
	RXA(
   kcstr.buf = 
		kcidxget(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&kcstr.size
		);
	);
 	RET_OPT((kcstr.buf), copy_kcstring(&kcstr));
}

CAMLprim value
caml_kcidx_get_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	
	KCString kcstr;
	RXA(
   kcstr.buf = 
		kcidxget(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&kcstr.size
		);
	);
   if(!kcstr.buf) EXC_KEYNOTFOUND( _EXC("get") );
   CAMLreturn(copy_kcstring(&kcstr));
}

CAMLprim value
caml_kcidx_get_as_string(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size = 0;
	RXA(
   const char * r = 
		kcidxget(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&size
		);
	);
	if(r) {
		vret = copy_buffer_to_string(r, size);
		kcfree((char*)r);
	   CAMLreturn(Val_some(vret));
	} 
   CAMLreturn(Val_none);
}

CAMLprim value
caml_kcidx_get_as_string_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size = 0;
	RXA(
   const char * r = 
		kcidxget(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			&size
		);
	);
   if(!r) EXC_KEYNOTFOUND( _EXC("get_as_string") );
	vret = copy_buffer_to_string(r, size);
	kcfree((char*)r); 
   CAMLreturn( vret );
}

CAMLprim value
caml_kcidx_sync(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	int32_t r = 0;

   if (Is_long(arg3)) {
		RXA( 
			r =
			kcidxsync(
				KCIdx_val_p(arg1),
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
			kcidxsync(
				KCIdx_val_p(arg1),
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
caml_kcidx_sync_exc(value arg1, value arg2, value arg3, value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

	int32_t r = 0;

   if (Is_long(arg3)) {
		RXA( 
			r =
			kcidxsync(
				KCIdx_val_p(arg1),
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
			kcidxsync(
				KCIdx_val_p(arg1),
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
caml_kcidx_clear(value arg1) {
   CAMLparam1(arg1);

   RXA(const int32_t r = kcidxclear(KCIdx_val_p(arg1)));
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_clear_exc(value arg1) {
   CAMLparam1(arg1);

   RXA(const int32_t r = kcidxclear(KCIdx_val_p(arg1)));
   if(!r) caml_failwith( _EXC("clear") );
   RET_UNIT;
}

CAMLprim value
caml_kcidx_count(value arg1) {
   CAMLparam1(arg1);
	RXA(const int64_t sz = kcidxcount(KCIdx_val_p(arg1)));
 	RET_LONG(sz);
}

CAMLprim value
caml_kcidx_size(value arg1) {
   CAMLparam1(arg1);
	RXA(const int64_t sz = kcidxsize(KCIdx_val_p(arg1)));
 	RET_LONG(sz);
}

CAMLprim value
caml_kcidx_path(value arg1) {
   CAMLparam1(arg1);
	CAMLlocal1(vr);
	
	RXA(const char * r = kcidxpath(KCIdx_val_p(arg1)));
	vr = COPY_STRING_0(r);
	kcfree((void*)r);
	CAMLreturn(vr);
}

CAMLprim value
caml_kcidx_status(value arg1) {
	CAMLparam1(arg1);
	CAMLlocal1(vret);

   RXA(const char * r = kcidxstatus(KCIdx_val_p(arg1)));
	vret = r ? Val_some(caml_copy_string(r)) : Val_none;
	if(r) kcfree((void*)r);
 	CAMLreturn(vret);
}

CAMLprim value
caml_kcidx_setsub(value arg1, value arg2, value arg3, 
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
		kcidxset(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_setsub_exc(value arg1, value arg2, value arg3, 
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
		kcidxset(
			KCIdx_val_p(arg1),
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
caml_kcidx_settail(value arg1, value arg2, value arg3, 
	value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
	const size_t len = pkcstr->size - (size_t)n;
	RXA( 
   const int32_t r = 
		kcidxset(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_settail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("settail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
   const int32_t r = 
		kcidxset(
			KCIdx_val_p(arg1),
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
caml_kcidx_addsub(value arg1, value arg2, value arg3, 
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
		kcidxadd(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_addsub_exc(value arg1, value arg2, value arg3, 
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
		kcidxadd(
			KCIdx_val_p(arg1),
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
caml_kcidx_addtail(value arg1, value arg2, value arg3, 
	value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
	const size_t len = pkcstr->size - (size_t)n;
	RXA( 
   const int32_t r = 
		kcidxadd(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_addtail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("addtail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
   const int32_t r = 
		kcidxadd(
			KCIdx_val_p(arg1),
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
caml_kcidx_replace_with_sub(value arg1, value arg2, value arg3, 
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
		kcidxreplace(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_replace_with_sub_exc(value arg1, value arg2, value arg3, 
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
		kcidxreplace(
			KCIdx_val_p(arg1),
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
caml_kcidx_replace_with_tail(value arg1, value arg2, value arg3, 
	value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
	const size_t len = pkcstr->size - (size_t)n;
	RXA( 
   const int32_t r = 
		kcidxreplace(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_replace_with_tail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("replace_with_tail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
   const int32_t r = 
		kcidxreplace(
			KCIdx_val_p(arg1),
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
caml_kcidx_appendsub(value arg1, value arg2, value arg3, 
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
		kcidxappend(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_appendsub_exc(value arg1, value arg2, value arg3, 
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
		kcidxappend(
			KCIdx_val_p(arg1),
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
caml_kcidx_appendtail(value arg1, value arg2, value arg3, 
	value arg4) {
	CAMLparam4(arg1,arg2,arg3,arg4);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false);
	const size_t len = pkcstr->size - (size_t)n;
	RXA( 
   const int32_t r = 
		kcidxappend(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kcidx_appendtail_exc(value arg1, value arg2, value arg3, 
	value arg4, value arg5) {
	CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg3);
	const int n = Int_val(arg4);
   if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("appendtail") ); 
	const size_t len = pkcstr->size - (size_t)n;
	RXA(
   const int32_t r = 
		kcidxappend(
			KCIdx_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			pkcstr->buf + n,
			len
		)
	);
   if(!r) caml_failwith( _EXC("appendtail") );
   RET_UNIT;
}

extern /* -> kcdb.c */
CAMLprim value 
value_of_KCDB_nofinal(const KCDB*);


CAMLprim value
caml_kcidx_inner_db_op(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);
   CAMLlocal2 (vdb, vret);

	KCIdx * pidx = KCIdx_val(arg1);
	assert(pidx);
	KCDB * db = kcidxrevealinnerdb(pidx->p);
   if(!db) caml_failwith( _EXC("inner_db_op") );
	vdb = value_of_KCDB_nofinal(db);
	vret = caml_callback_exn(arg2, vdb);
	if(Is_exception_result(vret)) {
		RET_UNIT;
	}
   CAMLreturn(Val_some(vret));
}



CAMLprim value
caml_kcidx_inner_db_op_exc(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);
   CAMLlocal2 (vdb, vret);

	KCIdx * pidx = KCIdx_val(arg1);
	assert(pidx);
	KCDB * db = kcidxrevealinnerdb(pidx->p);
   if(!db) caml_failwith( _EXC("inner_db_op") );

	vdb = value_of_KCDB_nofinal(db);
	vret = caml_callback(arg2, vdb);
	CAMLreturn(vret);
}

