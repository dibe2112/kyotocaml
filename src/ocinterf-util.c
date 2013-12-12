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

#include "ocinterf-util.h"

/* =========================================== */

CAMLprim value
Val_some(value v) {   
    CAMLparam1( v );
    CAMLlocal1( some );
    some = caml_alloc(1, 0);
    Store_field( some, 0, v );
    CAMLreturn( some );
}

CAMLprim value
copy_buffer_to_string(const char * p, size_t size) {
	CAMLparam0();
	CAMLlocal1(vret);

	assert(p);
	vret = caml_alloc_string(size);
	memcpy(String_val(vret), (const void*)p, size);
	CAMLreturn(vret);
}

CAMLprim value
mkTpl2(value v1, value v2) {
	CAMLparam2(v1, v2);
	CAMLlocal1(vret);

	vret = caml_alloc_tuple(2);
	Store_field(vret, 0, v1);
	Store_field(vret, 1, v2);
   CAMLreturn(vret);
}

CAMLprim value
mkTpl3(value v1, value v2, value v3) {
	CAMLparam3(v1, v2, v3);
	CAMLlocal1(vret);

	vret = caml_alloc_tuple(3);
	Store_field(vret, 0, v1);
	Store_field(vret, 1, v2);
	Store_field(vret, 2, v3);
   CAMLreturn(vret);
}

CAMLprim value
mkTpl4(value v1, value v2, value v3, value v4) {
	CAMLparam4(v1, v2, v3, v4);
	CAMLlocal1(vret);

	vret = caml_alloc_tuple(4);
	Store_field(vret, 0, v1);
	Store_field(vret, 1, v2);
	Store_field(vret, 2, v3);
	Store_field(vret, 3, v4);
   CAMLreturn(vret);
}

CAMLprim value
mkTpl5(value v1, value v2, value v3, value v4, value v5) {
	CAMLparam5(v1, v2, v3, v4, v5);
	CAMLlocal1(vret);

	vret = caml_alloc_tuple(5);
	Store_field(vret, 0, v1);
	Store_field(vret, 1, v2);
	Store_field(vret, 2, v3);
	Store_field(vret, 3, v4);
	Store_field(vret, 4, v5);
   CAMLreturn(vret);
}

int length_of_list(value list) {
	int len = 0;
	while(list != Val_emptylist) {
		len++;
		list = Field(list, 1);
	}
	return len;
}
