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
#include <kclangc.h>
#include "kccur.h"  /* Include after kclangc.h */

/*********************************************************************/

#define KCCur_val(V) ((KCCur *) Data_custom_val( Field(V, 0) ))
#define KCCur_val_p(V) ((KCCur_val(V))->p)

#define KCCustCur_val(V) ((KCCur *) Data_custom_val(V))
#define KCCustCur_val_p(V) ((KCCustCur_val(V))->p)


#define EXC_KEYNOTFOUND(MSG) raise_keynotfound(MSG)

#undef _EXC
#define _EXC(A) "kccur." A

#undef KCCUR_IDENTIFIER
#define KCCUR_IDENTIFIER "kyotOCAml.kccur.1.0"

#ifdef FINMGS_CUR
#define FINALMSG(S,A) printf(S "{%#lx}\n", (unsigned long)(A)); fflush(stdout);
#else
#define FINALMSG(S,A)
#endif

extern void raise_keynotfound(const char*);

/*********************************************************************/
static size_t
sizeofKCCur = sizeof(KCCur);

static void 
_custom_finalize_kccur(value vobj) {
	KCCur * pcur = (KCCur *) Data_custom_val(vobj);
	assert(pcur);
	assert(pcur->p);
	FINALMSG("**KCCUR object to delete", pcur->p);
	kccurdel(pcur->p);
	FINALMSG("**KCCUR object was deleted", 0);
}

static struct custom_operations 
_custops_kccur = {
  identifier: KCCUR_IDENTIFIER,
  finalize: _custom_finalize_kccur,
  compare: custom_compare_default,
  hash: custom_hash_default,
  serialize: custom_serialize_default,
  deserialize: custom_deserialize_default 
};

/*********************************************************************/
CAMLprim value 
copy_kccur(const KCCur * pcur) {
   CAMLparam0 ();
   CAMLlocal1 (vret);

   vret = caml_alloc_custom( &_custops_kccur, sizeofKCCur, 0, 1);
   memcpy( (void*)Data_custom_val(vret), (const void*)pcur, sizeofKCCur);
   CAMLreturn(vret);
}

CAMLprim value
caml_kccur_setvalue(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg2);
	assert(pkcstr->buf);
   RXA(
	const int32_t r =
		kccursetvalue(
			KCCur_val_p(arg1),
			pkcstr->buf,
			pkcstr->size,
			(int32_t)Bool_val(arg3)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kccur_setvalue_exc(value arg1, value arg2, value arg3) {
	CAMLparam3(arg1,arg2,arg3);

   KCString * pkcstr = KCString_val(arg2);
   assert(pkcstr->buf);
   RXA(
	const int32_t r =
		kccursetvalue(
			KCCur_val_p(arg1),
			pkcstr->buf,
			pkcstr->size,
			(int32_t)Bool_val(arg3)
		)
	);
   if(!r) caml_failwith( _EXC("setvalue") );
   RET_UNIT;
}

CAMLprim value
caml_kccur_setvalue_string(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1,arg2,arg3);

   RXA(
        const int32_t r =
		kccursetvalue(
			KCCur_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			(int32_t)Bool_val(arg3)
		)
        );
   RET_BOOL(r);
}

CAMLprim value
caml_kccur_setvalue_string_exc(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1,arg2,arg3);

   RXA(
   const int32_t r =
		kccursetvalue(
			KCCur_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2),
			(int32_t)Bool_val(arg3)
		)
	);
   if(!r) caml_failwith( _EXC("setvalue_string") );
   RET_UNIT;
}

CAMLprim value
caml_kccur_remove(value arg1) {
	CAMLparam1(arg1);
	RXA( const int32_t r = kccurremove(KCCur_val_p(arg1)) );
	RET_BOOL(r);
}

CAMLprim value
caml_kccur_remove_exc(value arg1) {
   CAMLparam1(arg1);
   RXA( const int32_t r = kccurremove(KCCur_val_p(arg1)) );
   if(!r) caml_failwith( _EXC("remove") );
   RET_UNIT;
}

CAMLprim value
caml_kccur_getkey(value arg1, value arg2) {
   CAMLparam2(arg1,arg2);

   KCString kstr;
   RXA( 
	kstr.buf =
		kccurgetkey(
			KCCur_val_p(arg1),
			&kstr.size,
			(int32_t)Bool_val(arg2)
		)
   );
   RET_OPT((kstr.buf), copy_kcstring(&kstr));
}

CAMLprim value
caml_kccur_getkey_as_string(value arg1, value arg2) {
   CAMLparam2(arg1,arg2);
   CAMLlocal1(vret);

   size_t size;
   RXA( 
	const char * r =
		kccurgetkey(
			KCCur_val_p(arg1),
			&size,
			(int32_t)Bool_val(arg2)
		)
	);
   if(!r) RET_NONE;
   vret = copy_buffer_to_string(r, size);
   kcfree((char*)r); 
   RET_SOME(vret);
}

CAMLprim value
caml_kccur_getkey_exc(value arg1, value arg2) {
   CAMLparam2(arg1,arg2);

   KCString kstr;
   kstr.buf = kccurgetkey(KCCur_val_p(arg1), &kstr.size, (int32_t)Bool_val(arg2));
	if(!kstr.buf) caml_failwith( _EXC("getkey")); 
   CAMLreturn(copy_kcstring(&kstr));
}

CAMLprim value
caml_kccur_getkey_as_string_exc(value arg1, value arg2) {
   CAMLparam2(arg1,arg2);
   CAMLlocal1(vret);

   size_t size;
   RXA( 
	const char * r =
	kccurgetkey(
          KCCur_val_p(arg1),
          &size,
          (int32_t)Bool_val(arg2)
      )
   );
   if(!r) caml_failwith( _EXC("getkey_as_string")); 
   vret = copy_buffer_to_string(r, size);
   kcfree((char*)r); 
   CAMLreturn(vret);
}

CAMLprim value
caml_kccur_getvalue(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);

   KCString kstr;
   RXA( 
   kstr.buf =
		kccurgetvalue(
			KCCur_val_p(arg1),
			&kstr.size,
			(int32_t)Bool_val(arg2)
		)
	);
   RET_OPT((kstr.buf), copy_kcstring(&kstr));
}

CAMLprim value
caml_kccur_getvalue_as_string(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size;
   RXA( 
   const char * r =
		kccurgetvalue(
			KCCur_val_p(arg1),
			&size,
			(int32_t)Bool_val(arg2)
		)
	);
   if(!r) RET_NONE;
	vret = copy_buffer_to_string(r, size);
	//kcfree((char*)r); 
   RET_SOME(vret);
}

CAMLprim value
caml_kccur_getvalue_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);

   KCString kstr;
   RXA( 
   kstr.buf =
		kccurgetvalue(
			KCCur_val_p(arg1),
			&kstr.size,
			(int32_t)Bool_val(arg2)
		)
	);
	if(!kstr.buf) caml_failwith( _EXC("getvalue") ); 
   CAMLreturn(copy_kcstring(&kstr));
}

CAMLprim value
caml_kccur_getvalue_as_string_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

   size_t size;
   RXA( 
   const char * r =
		kccurgetvalue(
			KCCur_val_p(arg1),
			&size,
			(int32_t)Bool_val(arg2)
		)
	);
	if(!r) caml_failwith( _EXC("getvalue_as_string") ); 
	vret = copy_buffer_to_string(r, size);
	kcfree((char*)r); 
   CAMLreturn(vret);
}

CAMLprim value
caml_kccur_get(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);

   KCString keykstr, valkstr;
   RXA(
   keykstr.buf =
		kccurget(
			KCCur_val_p(arg1),
			&keykstr.size,
			(const char**)&valkstr.buf,
			&valkstr.size,
			(int32_t)Bool_val(arg2)
		)
	);
   RET_OPT(keykstr.buf, mkTpl2(copy_kcstring(&keykstr), copy_kcstring_nofree(&valkstr) ));
}

CAMLprim value
caml_kccur_get_as_string(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

	const char * valbuf;
	size_t keysize, valsize;
   RXA(
	const char * r =
		kccurget(
			KCCur_val_p(arg1),
			&keysize,
			(const char**)&valbuf,
			&valsize,
			(int32_t)Bool_val(arg2)
		)
	);
   if(!r) RET_NONE;
	vret = mkTpl2(copy_buffer_to_string(r, keysize), copy_buffer_to_string(valbuf, valsize));
	kcfree((char*)r); 
   RET_SOME(vret);
}

CAMLprim value
caml_kccur_get_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);

   KCString keykstr, valkstr;
   RXA(
	keykstr.buf =
		kccurget(
			KCCur_val_p(arg1),
			&keykstr.size,
			(const char**)&valkstr.buf,
			&valkstr.size,
			(int32_t)Bool_val(arg2)
		)
	);
	if(!keykstr.buf) caml_failwith( _EXC("get") ); 
   CAMLreturn(mkTpl2(copy_kcstring(&keykstr), copy_kcstring_nofree(&valkstr)));
}

CAMLprim value
caml_kccur_get_as_string_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	CAMLlocal1(vret);

	const char * valbuf;
	size_t keysize, valsize;
   RXA(
	const char * r =
		kccurget(
			KCCur_val_p(arg1),
			&keysize,
			(const char**)&valbuf,
			&valsize,
			(int32_t)Bool_val(arg2)
		)
	);
	if(!r) caml_failwith( _EXC("get_as_string") ); 
	vret = mkTpl2(copy_buffer_to_string(r, keysize), copy_buffer_to_string(valbuf, valsize));
	kcfree((char*)r); 
   CAMLreturn(vret);
}

CAMLprim value
caml_kccur_seize(value arg1) {
	CAMLparam1(arg1);

   KCString keykstr, valkstr;
   RXA(
	keykstr.buf =
		kccurseize(
			KCCur_val_p(arg1),
			&keykstr.size,
			(const char**)&valkstr.buf,
			&valkstr.size
		)
	);
   RET_OPT((keykstr.buf), mkTpl2(copy_kcstring(&keykstr), copy_kcstring_nofree(&valkstr)));
}

CAMLprim value
caml_kccur_seize_as_string(value arg1) {
	CAMLparam1(arg1);
	CAMLlocal1(vret);

	const char * valbuf;
	size_t keysize, valsize;
	RXA(
	const char * r =
		kccurseize(
			KCCur_val_p(arg1),
			&keysize,
			(const char**)&valbuf,
			&valsize
		)
	);
	if(!r) RET_NONE;
	vret = mkTpl2(copy_buffer_to_string(r, keysize), copy_buffer_to_string(valbuf, valsize));
	kcfree((char*)r); 
	RET_SOME(vret);
}

CAMLprim value
caml_kccur_seize_exc(value arg1) {
	CAMLparam1(arg1);

	KCString keykstr, valkstr;
	RXA(
	keykstr.buf =
		kccurseize(
			KCCur_val_p(arg1),
			&keykstr.size,
			(const char**)&valkstr.buf,
			&valkstr.size
		)
	);
	if(!keykstr.buf) caml_failwith( _EXC("seize") ); 
	CAMLreturn(mkTpl2(copy_kcstring(&keykstr), copy_kcstring_nofree(&valkstr)));
}

CAMLprim value
caml_kccur_seize_as_string_exc(value arg1) {
	CAMLparam1(arg1);
	CAMLlocal1(vret);

	const char * valbuf;
	size_t keysize, valsize;
	RXA(
	const char * r =
		kccurseize(
			KCCur_val_p(arg1),
			&keysize,
			(const char**)&valbuf,
			&valsize
		)
	);
	if(!r) caml_failwith( _EXC("seize_as_string") ); 
	vret = mkTpl2(copy_buffer_to_string(r, keysize), copy_buffer_to_string(valbuf, valsize));
	kcfree((char*)r); 
	CAMLreturn(vret);
}

CAMLprim value
caml_kccur_jump(value arg1) {
	CAMLparam1(arg1);
	const int32_t r = kccurjump(KCCur_val_p(arg1));
	RET_BOOL(r);
}

CAMLprim value
caml_kccur_jump_exc(value arg1) {
	CAMLparam1(arg1);
   RXA( const int32_t r = kccurjump(KCCur_val_p(arg1)) );
	if(!r) caml_failwith( _EXC("jump") );
	RET_UNIT;
}

CAMLprim value
caml_kccur_jumpkey(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	RXA(
	const int32_t r =
		kccurjumpkey(
			KCCur_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
	RET_BOOL(r);
}

CAMLprim value
caml_kccur_jumpkey_exc(value arg1, value arg2) {
	CAMLparam2(arg1,arg2);
	RXA(
	const int32_t r =
		kccurjumpkey(
			KCCur_val_p(arg1),
			String_val(arg2),
			caml_string_length(arg2)
		)
	);
	if(!r) EXC_KEYNOTFOUND("jumpkey");
	RET_UNIT;
}

CAMLprim value
caml_kccur_jumpback(value arg1) {
   CAMLparam1(arg1);
   RXA( const int32_t r = kccurjumpback(KCCur_val_p(arg1)) );
   RET_BOOL(r);
}

CAMLprim value
caml_kccur_jumpback_exc(value arg1) {
   CAMLparam1(arg1);
   RXA( const int32_t r = kccurjumpback(KCCur_val_p(arg1)) );
   if(!r) caml_failwith( _EXC("jumpback") );
   RET_UNIT;
}

CAMLprim value
caml_kccur_step(value arg1) {
   CAMLparam1(arg1);
   RXA( const int32_t r = kccurstep(KCCur_val_p(arg1)) );
   RET_BOOL(r);
}

CAMLprim value
caml_kccur_step_exc(value arg1) {
   CAMLparam1(arg1);
   RXA( const int32_t r = kccurstep(KCCur_val_p(arg1)) );
   if(!r) caml_failwith( _EXC("step") );
   RET_UNIT;
}

CAMLprim value
caml_kccur_stepback(value arg1) {
	CAMLparam1(arg1);
   RXA( const int32_t r = kccurstepback(KCCur_val_p(arg1)) );
	RET_BOOL(r);
}

CAMLprim value
caml_kccur_stepback_exc(value arg1) {
	CAMLparam1(arg1);
   RXA( const int32_t r = kccurstepback(KCCur_val_p(arg1)) );
	if(!r) caml_failwith( _EXC("stepback") );
	RET_UNIT;
}

CAMLprim value
caml_kccur_ecode(value arg1) {
	CAMLparam1(arg1);
   const int32_t r = kccurecode(KCCur_val_p(arg1));
   RET_INT(r);
}

CAMLprim value
caml_kccur_emsg(value arg1) {
	CAMLparam1(arg1);
   const char * r = kccuremsg(KCCur_val_p(arg1));
   CAMLreturn( COPY_STRING_0(r) );
}

/*********************************************************************/

CAMLprim value
caml_kccur_setsubvalue(value arg1, value arg2, value arg3
,value arg4, value arg5) {
   CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg2);
   assert(pkcstr->buf);
   const int n = Int_val(arg3);
   if(n < 0 || n >= pkcstr->size) CAMLreturn(Val_false); 
   const size_t len = (size_t)Int_val(arg4);
   if(n + len < 0 || n + len >= pkcstr->size) CAMLreturn(Val_false);
   RXA(
	const int len_positive = len >= 0;  
	const int32_t r =
		kccursetvalue(
			KCCur_val_p(arg1),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len,
			(int32_t)Bool_val(arg5)
		)
	);
   RET_BOOL(r);
}

CAMLprim value
caml_kccur_setsubvalue_exc(value arg1, value arg2, value arg3
	, value arg4, value arg5) {
   CAMLparam5(arg1,arg2,arg3,arg4,arg5);

   KCString * pkcstr = KCString_val(arg2);
   assert(pkcstr->buf);
   const int n = Int_val(arg3);
   if(n < 0 || n >= pkcstr->size) caml_invalid_argument( _EXC("setsubvalue") ); 
   const size_t len = (size_t)Int_val(arg4);
   if(n + len < 0 || n + len >= pkcstr->size) caml_invalid_argument( _EXC("setsubvalue") );
   RXA(
	const int len_positive = len >= 0;  
	const int32_t r =
		kccursetvalue(
			KCCur_val_p(arg1),
			len_positive ? pkcstr->buf + n : pkcstr->buf + n + len,
			len_positive ? len : -len,
			(int32_t)Bool_val(arg5)
		)
	);
   if(!r) caml_failwith( _EXC("setsubvalue") );
   RET_UNIT;
}


