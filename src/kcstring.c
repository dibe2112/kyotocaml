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

#define _GNU_SOURCE

#include <assert.h>
#include <string.h> 
#include <stdio.h>
#include <caml/memory.h> 
#include <caml/alloc.h> 
#include <caml/callback.h> 
#include <caml/custom.h> 
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/signals.h>

#include "ocinterf-util.h"
#include "kcstring.h" 
#include <kclangc.h>


/* ********************************************************* */
#define MIN_LEN(S1,S2) ((S1).size <= (S2).size ? (S1).size : (S2).size)  

#undef _EXC
#define _EXC(A) "kcstring." A

#undef KCSTRING_IDENTIFIER
#define KCSTRING_IDENTIFIER "kyotOCAml.kcstring.1.0"


#ifdef FINMGS_STR
#define FINALMSG(S,A) printf(S "{%#lx}\n", (unsigned long)(A)); fflush(stdout);
#else
#define FINALMSG(S,A)
#endif

static const size_t
sizeofKCString = sizeof(KCString);

/* ********************************************************* */

static void 
_custom_finalize_kcstring(value vobj) {
	KCString * pkcs = KCString_val(vobj);
	assert(pkcs);
  	assert(pkcs->buf);
	FINALMSG("**KCString object to delete", pkcs->buf);
  	kcfree(pkcs->buf);
	FINALMSG("**KCString object was deleted", 0);
}

static void 
_custom_finalize_kcstring_nofree(value vobj) {
	//assert( 0 < printf("kcstring object was deleted (nofree)\n") );
}

static int
_custom_compare_kcstring(value v1, value v2) {
  KCString * pkcs1 = (KCString *) Data_custom_val(v1);
  KCString * pkcs2 = (KCString *) Data_custom_val(v2);
  assert(pkcs1);
  assert(pkcs2);
  assert(pkcs1->buf);
  assert(pkcs2->buf);
  const int r = strncmp(pkcs1->buf, pkcs2->buf, MIN_LEN(*pkcs1,*pkcs2));
  if(r || pkcs1->size == pkcs2->size) return r;
  if(pkcs1->size > pkcs2->size) return 1;
  return -1;
}

static intnat
_custom_hash_kcstring(value v) {
   #define LEN  (sizeof(uint64_t)/sizeof(intnat)) 
	
	union {
		intnat   intn[LEN];
		uint64_t ui64;
	} uni; 
	KCString * pkcs = (KCString*) Data_custom_val(v);
	intnat hashv = (intnat)0x0000;
	uni.ui64 = kchashmurmur(pkcs->buf, pkcs->size);
	if(LEN == 1)
		hashv = uni.intn [0];	
	else if (LEN == 2)
		hashv = uni.intn [0] ^ uni.intn [1];	
	else
		assert(0);	
  return hashv;
  #undef SIZE_OF_INTNAT
}

static void
_custom_serialize_kcstring(value v, uintnat * wsize_32, uintnat * wsize_64) {
  KCString * pkcs = (KCString *) Data_custom_val(v);
  assert(pkcs);
  assert(pkcs->buf);
  caml_serialize_block_1(&pkcs->size, sizeof pkcs->size);
  caml_serialize_block_1(pkcs->buf, pkcs->size);
  *wsize_32 = *wsize_64 = sizeof (*pkcs);
}

static uintnat
_custom_deserialize_kcstring(void * dst) {
	KCString * pkcs = (KCString *) dst;
 
	caml_deserialize_block_1(&pkcs->size, sizeof pkcs->size);
   pkcs->buf = kcmalloc(pkcs->size);
   if(!pkcs->buf) caml_deserialize_error("Out of memory");
	caml_deserialize_block_1(pkcs->buf, pkcs->size);
   return (uintnat) (sizeofKCString);
}

static struct custom_operations 
_custops_kcstring = {
  identifier: KCSTRING_IDENTIFIER,
  finalize: _custom_finalize_kcstring,
  compare: _custom_compare_kcstring,
  hash: _custom_hash_kcstring,
  serialize: _custom_serialize_kcstring,
  deserialize: _custom_deserialize_kcstring 
};

static struct custom_operations 
_custops_kcstring_nofree = {
  identifier: KCSTRING_IDENTIFIER,
  finalize: _custom_finalize_kcstring_nofree,
  compare: _custom_compare_kcstring,
  hash: _custom_hash_kcstring,
  serialize: _custom_serialize_kcstring,
  deserialize: _custom_deserialize_kcstring 
};

/* ********************************************************* */

__attribute__((constructor)) void init_kcstring(void) { 
	caml_register_custom_operations(&_custops_kcstring);
}

CAMLprim value 
copy_kcstring(const KCString * pkcs) {
   CAMLparam0 ();
   CAMLlocal1 (vret);

   vret = caml_alloc_custom( &_custops_kcstring, sizeofKCString, 0, 1);
   memcpy( (void*)Data_custom_val(vret), (void*)pkcs, sizeofKCString);
   CAMLreturn(vret);
}

CAMLprim value 
copy_kcstring_nofree(const KCString * pkcs) {
   CAMLparam0 ();
   CAMLlocal1 (vret);

   vret = caml_alloc_custom( &_custops_kcstring_nofree, sizeofKCString, 0, 1);
   memcpy( (void*)Data_custom_val(vret), (void*)pkcs, sizeofKCString);
   CAMLreturn(vret);
}

CAMLprim value
copy_buffer_to_kcstring(const char * buf, size_t size) {
	CAMLparam0 ();
	CAMLlocal1 (vret);

	union {
		void * pv;
		KCString *ps;
	} un;

	vret = caml_alloc_custom( &_custops_kcstring, sizeofKCString, 0, 1);
	un.pv = (void*)Data_custom_val(vret);
	un.ps->buf = (char*)buf;
	un.ps->size = size;
	CAMLreturn(vret);
}

CAMLprim value
copy_buffer_to_kcstring_nofree(const char * buf, size_t size) {
   CAMLparam0 ();
   CAMLlocal1 (vret);

	union {
		void * pv;
		KCString *ps;
	} un;

	vret = caml_alloc_custom( &_custops_kcstring_nofree, sizeofKCString, 0, 1);
	un.pv = (void*)Data_custom_val(vret);
	un.ps->buf = (char*)buf;
	un.ps->size = size;
	CAMLreturn(vret);
}

void 
free_buf(value vkcstr) {
	CAMLparam1(vkcstr);
	
	char * p = KCString_val_buf(vkcstr);
	KCString_val_buf(vkcstr) = NULL;
	KCString_val_size(vkcstr) = 0;
	kcfree(p);
	CAMLreturn0;
}

CAMLprim value
caml_kcstring_create(value arg1) {
   CAMLparam1(arg1);
	
   const int size = Int_val(arg1);
   if(size < 0) caml_invalid_argument( _EXC("make") );
   const char * const p = kcmalloc((size_t)size);
   if(NULL == p) caml_raise_out_of_memory();
   const KCString kcs = { (char*)p, (size_t)size };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_make(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);
	
   const int size = Int_val(arg1);
   if(size < 0) caml_invalid_argument( _EXC("create") );
   char * const p = kcmalloc((size_t)size);
   if(NULL == p) caml_raise_out_of_memory();
   memset(p, (char)Int_val(arg2), (size_t)size);
   KCString kcs = { (char*)p, (size_t)size };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_copy(value arg1) {
   CAMLparam1(arg1);

   KCString * pkcs = KCString_val(arg1);
   char * const p = kcmalloc( pkcs->size );
   if(NULL == p) caml_raise_out_of_memory();
   memcpy(p, pkcs->buf, pkcs->size);
   KCString kcs = { (char*)p, (size_t)pkcs->size };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_enlarge(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);
	
	const char * buf = 0;
	KCString * pkcs = KCString_val(arg1);
	const int iarg2 = Int_val(arg2);
	const int ispos = iarg2 >= 0;
	const int new_size =  ispos ? iarg2 : -iarg2;
	if((size_t)new_size < pkcs->size) caml_invalid_argument( _EXC("enlarge") );
	if(new_size > pkcs->size) {
	   char * const p = kcmalloc( new_size );
	   if(NULL == p) caml_raise_out_of_memory();
		if(ispos)
	   	memcpy(p, pkcs->buf, pkcs->size );
		else
	   	memcpy(p + (new_size - pkcs->size), pkcs->buf, pkcs->size);
		buf = pkcs->buf; 
		pkcs->buf = p;
		pkcs->size = new_size;
		kcfree((void*)buf);
	}
   RET_UNIT;
}
 
CAMLprim value
caml_kcstring_of_string(value arg1) {
   CAMLparam1(arg1);
	
   size_t size = caml_string_length(arg1);
   char * const p = kcmalloc(size);
   if(NULL == p) caml_raise_out_of_memory();
   memcpy(p, String_val(arg1), size);
   KCString kcs = { (char*)p, (size_t)size };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_to_string(value arg1) {
   CAMLparam1(arg1);
	CAMLlocal1(vr);
   const char * p = NULL;

   KCString * pkcs = KCString_val(arg1);
   vr = caml_alloc_string(pkcs->size);
   p = String_val(vr);
   memcpy((void*)p, pkcs->buf, pkcs->size);
   CAMLreturn(vr);
}

CAMLprim value
caml_kcstring_of_int(value arg1) {
	CAMLparam1(arg1);

	char buffer[40];
	sprintf(buffer, "%d", (int)Int_val(arg1));
	const int len = strlen(buffer);
	assert(len < sizeof buffer);
	char * const p = kcmalloc(len);
	if(!p) caml_raise_out_of_memory();
	memcpy(p, buffer, len);
   KCString kcs = { (char*)p, (size_t)len };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_of_int64(value arg1) {
	CAMLparam1(arg1);

	char buffer[40];
	sprintf(buffer, "%ld", (int64_t)Int64_val(arg1));
	const int len = strlen(buffer);
	assert(len < sizeof buffer);
	char * const p = kcmalloc(len);
	if(!p) caml_raise_out_of_memory();
	memcpy(p, buffer, len);
   KCString kcs = { (char*)p, (size_t)len };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_of_int32(value arg1) {
	CAMLparam1(arg1);

	char buffer[40];
	sprintf(buffer, "%d", (int)Int32_val(arg1));
	const int len = strlen(buffer);
	assert(len < sizeof buffer);
	char * const p = kcmalloc(len);
	if(!p) caml_raise_out_of_memory();
	memcpy(p, buffer, len);
   KCString kcs = { (char*)p, (size_t)len };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_of_intnat(value arg1) {
	CAMLparam1(arg1);

	char buffer[40];
	sprintf(buffer, "%d", (int)Nativeint_val(arg1));
	const int len = strlen(buffer);
	assert(len < sizeof buffer);
	char * const p = kcmalloc(len);
	if(!p) caml_raise_out_of_memory();
	memcpy(p, buffer, len);
   KCString kcs = { (char*)p, (size_t)len };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_of_float(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);
	char fmt_buffer[24];
	char buffer[40];
	
	const int digits = Int_val(arg2);
	const double d = Double_val(arg1);

	if(digits < 0 || digits > 16)  caml_invalid_argument( _EXC("of_float") );
	if( (d >= -1e10 && d <= -1e-10) || (d <= 1e10 && d >= 1e-10))
		sprintf(fmt_buffer, "%%.%df", digits);
	else
		sprintf(fmt_buffer, "%%.%de", digits);

	//printf("**%s**", fmt_buffer);
	sprintf(buffer, fmt_buffer, d);

	const int len = strlen(buffer);
	assert(len < sizeof buffer);
	char * const p = kcmalloc(len);
	if(!p) caml_raise_out_of_memory();
	memcpy(p, buffer, len);
   KCString kcs = { (char*)p, (size_t)len };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_compare_nocase(value arg1, value arg2) {
  CAMLparam2(arg1, arg2);

  KCString * pkcs1 = (KCString *) Data_custom_val(arg1);
  KCString * pkcs2 = (KCString *) Data_custom_val(arg2);
  assert(pkcs1);
  assert(pkcs1->buf);
  assert(pkcs2);
  assert(pkcs2->buf);

  const int r = strncasecmp(pkcs1->buf, pkcs2->buf, MIN_LEN(*pkcs1, *pkcs2) );
  if(r || pkcs1->size == pkcs2->size) RET_INT(r);
  if(pkcs1->size > pkcs2->size) RET_INT(1);
  RET_INT(-1);
}

CAMLprim value
caml_kcstring_compare(value arg1, value arg2) {
  CAMLparam2(arg1, arg2);

  KCString * pkcs1 = (KCString *) Data_custom_val(arg1);
  KCString * pkcs2 = (KCString *) Data_custom_val(arg2);
  assert(pkcs1);
  assert(pkcs1->buf);
  assert(pkcs2);
  assert(pkcs2->buf);

  const int r = strncmp(pkcs1->buf, pkcs2->buf, MIN_LEN(*pkcs1, *pkcs2) );
  if(r || pkcs1->size == pkcs2->size) RET_INT(r);
  if(pkcs1->size > pkcs2->size) RET_INT(1);
  RET_INT(-1);
}

CAMLprim value
caml_kcstring_compare_with_string(value arg1, value arg2) {
  CAMLparam2(arg1, arg2);

  KCString * pkcs = (KCString *) Data_custom_val(arg1);
  const size_t strlen_arg2 = caml_string_length(arg2);
  assert(pkcs);
  assert(pkcs->buf);

  const int r = strncmp(pkcs->buf, String_val(arg2), (pkcs->size < strlen_arg2 ? pkcs->size : strlen_arg2) );
  if(r || pkcs->size == strlen_arg2) RET_INT(r);
  if(pkcs->size > strlen_arg2) RET_INT(1);
  RET_INT(-1);
}

CAMLprim value
caml_kcstring_compare_nocase_with_string(value arg1, value arg2) {
  CAMLparam2(arg1, arg2);

  KCString * pkcs = (KCString *) Data_custom_val(arg1);
  const size_t strlen_arg2 = caml_string_length(arg2);
  assert(pkcs);
  assert(pkcs->buf);

  const int r = strncasecmp(pkcs->buf, String_val(arg2), (pkcs->size < strlen_arg2 ? pkcs->size : strlen_arg2) );
  if(r || pkcs->size == strlen_arg2) RET_INT(r);
  if(pkcs->size > strlen_arg2) RET_INT(1);
  RET_INT(-1);
}

CAMLprim value
caml_kcstring_length(value arg1) {
   CAMLparam1(arg1);

   KCString * pkcs = KCString_val(arg1);
   RET_INT((int)pkcs->size);
}

CAMLprim value
caml_kcstring_unsafe_get(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

   const KCString * pkcs = KCString_val(arg1);
   const int ndx = Int_val(arg2);
   assert(pkcs);
   assert(pkcs->buf);
   RET_INT(pkcs->buf[ndx]);
}

CAMLprim value
caml_kcstring_get(value arg1, value arg2) {
   CAMLparam2(arg1, arg2);

   const KCString * pkcs = KCString_val(arg1);
   const int ndx = Int_val(arg2);
   assert(pkcs);
   assert(pkcs->buf);
   if(ndx < 0  || ndx >= pkcs->size) caml_invalid_argument( _EXC("get") );
   RET_INT(pkcs->buf[ndx]);
}

CAMLprim value
caml_kcstring_unsafe_set(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

   const KCString * pkcs = KCString_val(arg1);
   const int ndx = Int_val(arg2);
   assert(pkcs);
   assert(pkcs->buf);
   pkcs->buf[ndx] = (char) Int_val(arg3);
   RET_UNIT;
}

CAMLprim value
caml_kcstring_set(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

   const KCString * pkcs = KCString_val(arg1);
   const int ndx = Int_val(arg2);
   assert(pkcs);
   assert(pkcs->buf);
   if(ndx < 0  || ndx >= pkcs->size) caml_invalid_argument(  _EXC("set") );
   pkcs->buf[ndx] = (char) Int_val(arg3);
   RET_UNIT;
}

CAMLprim value
caml_kcstring_append(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);

	KCString * pkcs1 = (KCString *) Data_custom_val(arg1);
	KCString * pkcs2 = (KCString *) Data_custom_val(arg2);

	const size_t bytes_to_alloc = pkcs1->size + pkcs2->size;
	const void * p = kcmalloc(bytes_to_alloc);
	if(!p) caml_raise_out_of_memory();
	memcpy((char*)p, pkcs1->buf, pkcs1->size); 
	memcpy((char*)(p + pkcs1->size), pkcs2->buf, pkcs2->size); 
	KCString kcs = { (char*)p, bytes_to_alloc };
	CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_append_to_string(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);
	CAMLlocal1(vret);

	KCString * pkcs1 = (KCString *) Data_custom_val(arg1);
	KCString * pkcs2 = (KCString *) Data_custom_val(arg2);

	const size_t bytes_to_alloc = pkcs1->size + pkcs2->size;
        vret = caml_alloc_string(bytes_to_alloc);

	memcpy(String_val(vret), pkcs1->buf, pkcs1->size); 
	memcpy(String_val(vret) + pkcs1->size, pkcs2->buf, pkcs2->size); 
	CAMLreturn(vret);
}

CAMLprim value
caml_kcstring_concat(value arg1, value arg2) {

	CAMLparam2(arg1, arg2);
   
	size_t bytes_to_alloc = 0;
	int len_of_list = 0;
	int loop;
	const char * sep = NULL;
	size_t size_of_sep;

	if(Tag_val(arg2) == Custom_tag) {
		const KCString * pkcs = KCString_val(arg2);
		sep = pkcs->buf;
		size_of_sep = pkcs->size;
	}
	else if(Tag_val(arg2) == String_tag) {
		sep = String_val(arg2);
		size_of_sep = caml_string_length(arg2);
	}
	else {
		assert(0);
		caml_failwith("concat: type arg2 has wrong type");
	}
	value v = arg1;
	while ( v != Val_emptylist ) {
        value head = Field(v, 0);
		bytes_to_alloc += KCString_val_size(head);
		v = Field(v, 1);
		len_of_list++;
	}
	if(len_of_list > 1) bytes_to_alloc += (len_of_list - 1) * size_of_sep; 
	char * const p = kcmalloc(bytes_to_alloc);
	if(!p) caml_raise_out_of_memory();
	char * pp = (char*)p;
	v = arg1;
	for (loop = len_of_list; loop > 0; loop--) {
		value head = Field(v, 0);
		const KCString * pkcstr = KCString_val(head);
		assert(v != Val_emptylist);
		assert( pp < p + bytes_to_alloc );
		memcpy(pp, pkcstr->buf, pkcstr->size);
		pp += pkcstr->size;
		if(loop > 1) {
			memcpy(pp, sep, size_of_sep);
			pp += size_of_sep;
		}
		v = Field(v, 1);
	}
	assert( Val_emptylist == v ) ;
	assert( pp == p + bytes_to_alloc );

	CAMLreturn(copy_buffer_to_kcstring(p, bytes_to_alloc) );
}

CAMLprim value
caml_kcstring_concat_to_string(value arg1, value arg2) {

	CAMLparam2(arg1, arg2);
	CAMLlocal1(vret);

	const KCString * pkcs = KCString_val(arg2);
	size_t bytes_to_alloc = 0;
	int len_of_list = 0;
	int loop;
	value v = arg1;
	while (v != Val_emptylist ) {
		value head = Field(v, 0);
		bytes_to_alloc += KCString_val_size(head);
		v = Field(v, 1);
		len_of_list++;
	}
	if(len_of_list > 1) bytes_to_alloc += (len_of_list - 1) * pkcs->size;
	vret = caml_alloc_string(bytes_to_alloc); 
	char * const p = String_val(vret);
	char * pp = (char*)p;
	v = arg1;
	for (loop = len_of_list; loop > 0; loop--) {
		value head = Field(v, 0);
		const KCString * pkcstr = KCString_val(head);
		assert(v != Val_emptylist);
		assert( pp < p + bytes_to_alloc);
		memcpy(pp, KCString_val_buf(head), pkcstr->size);
		pp += pkcstr->size;
		if(loop > 1) {
			memcpy(pp, pkcs->buf, pkcs->size);
			pp += pkcs->size;
		}
		v = Field(v, 1);
	}
	assert( Val_emptylist == v ) ;
	assert( pp == p + bytes_to_alloc );
	CAMLreturn(vret);
}

CAMLprim value
caml_kcstring_sub(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

   const KCString * pkcs = KCString_val(arg1);
   const int ndx = Int_val(arg2);
   const int len = Int_val(arg3);
   if(ndx < 0 || len < 0 || ndx + len > pkcs->size) 
	caml_invalid_argument( _EXC("sub") );
   char * const p = kcmalloc( len );
   if(!p) caml_raise_out_of_memory();
   memcpy(p, pkcs->buf + ndx, len); 
   KCString kcs = { (char*)p, (size_t)len };
   CAMLreturn(copy_kcstring(&kcs));
}

CAMLprim value
caml_kcstring_sub_as_string(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);
   CAMLlocal1(vret);

   const KCString * pkcs = KCString_val(arg1);
   const int ndx = Int_val(arg2);
   const int len = Int_val(arg3);
   if(ndx < 0 || len < 0 || ndx + len > pkcs->size) 
		caml_invalid_argument( _EXC("sub_as_string") );
   vret = caml_alloc_string( len );
	memcpy(String_val(vret), pkcs->buf  + ndx, len);	
   CAMLreturn(vret);
}

CAMLprim value
caml_kcstring_fill(value arg1, value arg2, value arg3, value arg4) {
   CAMLparam4(arg1, arg2, arg3, arg4);
   
   const KCString * pkcs = KCString_val(arg1);
   const int ndx = Int_val(arg2);
   if(ndx < 0 || ndx >= pkcs->size) caml_invalid_argument( _EXC("fill") );
   const int len = Int_val(arg3);
   if(len < 0 || ndx + len >= pkcs->size) caml_invalid_argument( _EXC("fill") );
   const char c = (char)Int_val(arg4);
   memset(pkcs->buf + ndx, c, (size_t)len);
   RET_UNIT;
}

CAMLprim value
caml_kcstring_find(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const int offset = Int_val(arg1);
	KCString * pkcs1 = (KCString *) Data_custom_val(arg2);
	KCString * pkcs2 = (KCString *) Data_custom_val(arg3);

   if(offset < 0 || pkcs1->size <= offset) caml_invalid_argument( _EXC("find") );
	const char * r = (char*)memmem(pkcs1->buf + offset, pkcs1->size - offset, pkcs2->buf, pkcs2->size);
	if(!r) 
		RET_INT(-1);
	else
		RET_INT((int)(r - pkcs1->buf));
}

CAMLprim value
caml_kcstring_find_string(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const int offset = Int_val(arg1);
	KCString * pkcs1 = (KCString *) Data_custom_val(arg2);

   if(offset < 0 || pkcs1->size <= offset) caml_invalid_argument( _EXC("find") );
	const char * r = (char*)memmem(pkcs1->buf + offset, pkcs1->size - offset, String_val(arg3), caml_string_length(arg3));
	if(!r) 
		RET_INT(-1);
	else
		RET_INT((int)(r - pkcs1->buf));
}

CAMLprim value
caml_kcstring_find_in_string(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const int offset = Int_val(arg1);
	KCString * pkcs = (KCString *) Data_custom_val(arg3);
	const char * str_arg2 = String_val(arg2);
	const size_t len_arg2 = caml_string_length(arg2);

   if(offset < 0 || len_arg2 <= offset) caml_invalid_argument( _EXC("find") );
	const char * r = (char*)memmem(str_arg2 + offset, len_arg2 - offset, pkcs->buf, pkcs->size);
	if(!r) 
		RET_INT(-1);
	else
		RET_INT((int)(r - str_arg2));
}

char * copy_to_cstring(const KCString * pkcs) {

	char * snew = kcmalloc(1 + pkcs->size);
	memcpy(snew, pkcs->buf, pkcs->size);
	snew[pkcs->size] = '\0';
	return snew;
}

CAMLprim value
caml_kcstring_find_nocase(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const int offset = Int_val(arg1);
	KCString * pkcs1 = (KCString *) Data_custom_val(arg2);
	KCString * pkcs2 = (KCString *) Data_custom_val(arg3);

	const char * s1 = copy_to_cstring(pkcs1);
	const char * s2 = copy_to_cstring(pkcs2);
   if(offset < 0 || pkcs1->size <= offset) caml_invalid_argument( _EXC("find") );
	const char * r = (char*)strcasestr(s1 + offset, s2);
	kcfree((void*)s1);
	kcfree((void*)s2);
	if(!r) 
		RET_INT(-1);
	else
		RET_INT((int)(r - pkcs1->buf));
}

CAMLprim value
caml_kcstring_find_nocase_string(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const int offset = Int_val(arg1);
	KCString * pkcs = (KCString *) Data_custom_val(arg2);

	const char * s1 = copy_to_cstring(pkcs);
   if(offset < 0 || pkcs->size <= offset) caml_invalid_argument( _EXC("find") );
	const char * r = (char*)strcasestr(s1 + offset, String_val(arg3));
	kcfree((void*)s1);
	if(!r) 
		RET_INT(-1);
	else
		RET_INT((int)(r - pkcs->buf));
}

CAMLprim value
caml_kcstring_find_nocase_in_string(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	const int offset = Int_val(arg1);
	KCString * pkcs = (KCString *) Data_custom_val(arg2);
	const char * s = copy_to_cstring(pkcs);

   if(offset < 0 || pkcs->size <= offset) caml_invalid_argument( _EXC("find") );
	const char * r = (char*)strcasestr(String_val(arg2) + offset, s);
	kcfree((void*)s);
	if(!r) 
		RET_INT(-1);
	else
		RET_INT((int)(r - pkcs->buf));
}

CAMLprim value
caml_kcstring_print(value arg1) {
	CAMLparam1(arg1);
   const KCString * pkcs = KCString_val(arg1);
	RXA(
		fwrite(pkcs->buf, sizeof(pkcs->buf[0]), pkcs->size, stdout);
		fflush(stdout);
	);
	RET_UNIT;
}

CAMLprim value
caml_kcstring_println(value arg1) {
	CAMLparam1(arg1);
   const KCString * pkcs = KCString_val(arg1);
	RXA(
		fwrite(pkcs->buf, sizeof(pkcs->buf[0]), pkcs->size, stdout);
		putc('\n', stdout);
		fflush(stdout);
	);
	RET_UNIT;
}

CAMLprim value
caml_kcstring_atoi(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);
	
	char buffer[32];
	const int offset = Int_val(arg1);
   const KCString * pkcs = KCString_val(arg2);
	const int bytes_to_copy = pkcs->size - offset;
	if(offset < 0 || offset >= pkcs->size) caml_invalid_argument( _EXC("atoi") );
	memcpy(buffer, pkcs->buf + offset, bytes_to_copy);
	buffer[bytes_to_copy] = '\0';
	const int64_t r = kcatoi(buffer);
	RET_LONG(r);
}

CAMLprim value
caml_kcstring_atoix(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);

	char buffer[32];
	const int offset = Int_val(arg1);
   const KCString * pkcs = KCString_val(arg2);
	const int bytes_to_copy = pkcs->size - offset;
	if(offset < 0 || offset >= pkcs->size) caml_invalid_argument( _EXC("atoix") );
	memcpy(buffer, pkcs->buf + offset, bytes_to_copy);
	buffer[bytes_to_copy] = '\0';
	const int64_t r = kcatoix(buffer);
	RET_LONG(r);
}

CAMLprim value
caml_kcstring_atof(value arg1, value arg2) {
	CAMLparam2(arg1, arg2);

	char buffer[40];
	const size_t sob = sizeof buffer;
	const int offset = Int_val(arg1);
   const KCString * pkcs = KCString_val(arg2);
	if(offset < 0 || offset >= pkcs->size) caml_invalid_argument( _EXC("atof") );
	memcpy(buffer, pkcs->buf + offset, sob - 1);
	buffer[sob-1] = '\0';
	const double r = kcatof(buffer);
	RET_FLOAT(r);
}

CAMLprim value
caml_kcstring_levdist(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

   const KCString * pkcs1 = KCString_val(arg1);
   const KCString * pkcs2 = KCString_val(arg2);
	RXA(
		const size_t dist = 
		kclevdist(
			pkcs1->buf,
			pkcs1->size,
			pkcs2->buf,
			pkcs2->size,
			(int32_t)Bool_val(arg3)
		);
	);
	RET_LONG((int64_t)dist);
}

CAMLprim value
caml_kcstring_levdist_from_string(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

   const KCString * pkcs = KCString_val(arg1);
	RXA(
		const size_t dist = 
		kclevdist(
			pkcs->buf,
			pkcs->size,
			String_val(arg2), 
			caml_string_length(arg2),
			(int32_t)Bool_val(arg3)
		);
	);
	RET_LONG((int64_t)dist);
}

CAMLprim value
caml_kcstring_levdist_of_strings(value arg1, value arg2, value arg3) {
   CAMLparam3(arg1, arg2, arg3);

	RXA(
		const size_t dist = 
		kclevdist(
			String_val(arg1), 
			caml_string_length(arg1),
			String_val(arg2), 
			caml_string_length(arg2),
			(int32_t)Bool_val(arg3)
		);
	);
	RET_LONG((int64_t)dist);
}

typedef union {
	int64_t i64;
	uint64_t ui64;
} uni64; 

CAMLprim value
caml_kcstring_hashmurmur(value arg1) {
	CAMLparam1(arg1);

   const KCString * pkcs = KCString_val(arg1);
	uni64 r;
	RXA( r.ui64 = kchashmurmur(pkcs->buf, pkcs->size) );
	RET_LONG(r.i64);	
}

CAMLprim value
caml_kcstring_hashmurmur_of_string(value arg1) {
	CAMLparam1(arg1);

	uni64 r;
	RXA( r.ui64 = kchashmurmur(String_val(arg1), caml_string_length(arg1)) );
	RET_LONG(r.i64);	
}

CAMLprim value
caml_kcstring_hashfnv(value arg1) {
	CAMLparam1(arg1);

   const KCString * pkcs = KCString_val(arg1);
	uni64 r;
	RXA( r.ui64 = kchashfnv(pkcs->buf, pkcs->size) );
	RET_LONG(r.i64);	
}

CAMLprim value
caml_kcstring_hashfnv_of_string(value arg1) {
	CAMLparam1(arg1);

	uni64 r;
	RXA( r.ui64 = kchashfnv(String_val(arg1), caml_string_length(arg1)) );
	RET_LONG(r.i64);	
}
