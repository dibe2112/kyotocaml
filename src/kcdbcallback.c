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
#include "kcdbcallback.h"
#include "kcstring.h" 
#include <kclangc.h>


const char*
visitfull_func(const char *kbuf, size_t ksiz, 
	const char *vbuf, size_t vsiz, size_t *sp, void *opq) {

	CAMLparam0();
	CAMLlocal4(vret, vkey, vval, vstr);
	
	assert(kbuf);
	assert(vbuf);
	assert(sp);

	CBSTRCT * p_cbs = (CBSTRCT *) opq;
	const char * r = NULL;
	assert(p_cbs);
	assert(p_cbs->vfuncfull);
	assert(p_cbs->vopt);

	AXR(
		vkey = copy_buffer_to_kcstring_nofree(kbuf, ksiz);
		vval = copy_buffer_to_kcstring_nofree(vbuf, vsiz);
		vret = 
		caml_callback3_exn(
			*(p_cbs->vfuncfull),  // callback-func
			vkey,
			vval,
			*(p_cbs->vopt)   // opaque
		);
		if(Is_exception_result(vret)) {
			vret = Extract_exception(vret);
			kcfree((void*)kbuf);
			kcfree((void*)vbuf);
			caml_raise(vret);
		}
		else {
			if (Is_long(vret)) {
				switch (Int_val(vret)) {
					case 0: r = (char*)KCVISNOP; break;
					case 1: r = (char*)KCVISREMOVE; break;
					default: assert(0);
				}
			}
			else {
				assert(0 == Tag_val(vret));
				vstr = Field(vret,0);
				r = KCString_val_buf(vstr);
				*sp = KCString_val_size(vstr);
			}
		}
	);
	CAMLreturnT(const char *, r);
}

const char*
visitfull_string_func(const char *kbuf, size_t ksiz, 
	const char *vbuf, size_t vsiz, size_t *sp, void *opq) {

	CAMLparam0();
	CAMLlocal4(vret, vkey, vval, vstr);

	assert(kbuf);
	assert(vbuf);
	assert(sp);
   
	CBSTRCT * p_cbs = (CBSTRCT *) opq;
	const char * r = NULL;
	assert(p_cbs);
	assert(p_cbs->vfuncfull);
	assert(p_cbs->vopt);
	vkey = copy_buffer_to_string(kbuf, ksiz);
	vval = copy_buffer_to_string(vbuf, vsiz);
	AXR(
		vret = 
			caml_callback3_exn(
			*(p_cbs->vfuncfull),
			vkey,
			vval,
			*(p_cbs->vopt)
		);
		if(Is_exception_result(vret)) {
			vret = Extract_exception(vret);
			r = KCVISNOP;
		}
		else {
			if (Is_long(vret)) {
				switch (Int_val(vret)) {
					case 0: r = KCVISNOP; break;
					case 1: r = KCVISREMOVE; break;
					default: assert(0);
				}
			}
			else {
				assert(0 == Tag_val(vret));
				vstr = Field(vret,0);
				r = KCString_val_buf(vstr);
				*sp = KCString_val_size(vstr);
			}
		}
	);
	CAMLreturnT(const char *, r);
}

const char*
visitempty_func(const char *kbuf, size_t ksiz, size_t *sp, void *opq) {

	CAMLparam0();
	CAMLlocal3(vret, vkey, vstr);

	CBSTRCT * p_cbs = (CBSTRCT *) opq;
	const char * r = NULL;
	assert(p_cbs);
	assert(p_cbs->vfuncempty);
	assert(p_cbs->vopt);
	vkey = copy_buffer_to_kcstring_nofree(kbuf, ksiz); 
	AXR(
		vret = 
		caml_callback2_exn(
			*(p_cbs->vfuncempty),  // callback-func
			vkey,
			*(p_cbs->vopt)   // opaque
		);
		if(Is_exception_result(vret)) {
			vret = Extract_exception(vret);
			r = (char*)KCVISNOP;
		}
		else {
			if (Is_long(vret)) {
				switch (Int_val(vret)) {
					case 0: r = (char*)KCVISNOP; break;
					case 1: r = (char*)KCVISREMOVE; break;
					default: assert(0);
				}
			}
			else {
				assert(0 == Tag_val(vret));
				vstr = Field(vret,0);
				r = KCString_val_buf(vstr);
				*sp = KCString_val_size(vstr);
			}
		}
	);
	CAMLreturnT(const char *, r);
}

const char*
visitempty_string_func(const char *kbuf, size_t ksiz, size_t *sp, void *opq) {
	CAMLparam0();
	CAMLlocal3(vret, vkey, vstr);

	CBSTRCT * p_cbs = (CBSTRCT *) opq;
	const char * r = NULL;
	assert(p_cbs);
	assert(p_cbs->vfuncempty);
	assert(p_cbs->vopt);
	vkey = copy_buffer_to_string(kbuf, ksiz);
 	AXR(
		vret = 
		caml_callback2_exn(
			*(p_cbs->vfuncempty),
			vkey,
			*(p_cbs->vopt)
		);
		if(Is_exception_result(vret)) {
			vret = Extract_exception(vret);
			r = KCVISNOP;
		}
		else {
			if (Is_long(vret)) {
	        	switch (Int_val(vret)) {
            	case 0: r = KCVISNOP; break;
            	case 1: r = KCVISREMOVE; break;
	            default: assert(0);
				}
			}
			else {
				assert(0 == Tag_val(vret));
				vstr = Field(vret,0);
		   	r = KCString_val_buf(vstr);
				*sp = KCString_val_size(vstr);
			}
		}
	);
	CAMLreturnT(const char *, r);
}

/*-------------------------------------------------------------------*/

int32_t
fileproc_func(const char * path, int64_t count, int64_t size, void * opq) {
	
	CAMLparam0();
	CAMLlocal1(vret);
	CAMLlocalN(vargs, 4);
	
	int32_t r;
	CBSTRCT * p_cbs = (CBSTRCT *) opq;
	assert(p_cbs);
	assert(p_cbs->vfuncfull);
	assert(p_cbs->vopt);
	vargs[0] = caml_copy_string(path);
	vargs[1] = caml_copy_int64(count);
	vargs[2] = caml_copy_int64(size);
	vargs[3] = *(p_cbs->vopt);
 	AXR(
		vret = 
		caml_callbackN_exn(
			*(p_cbs->vfuncfull),
			4,
			vargs
		);
		if(Is_exception_result(vret)) {
			vret = Extract_exception(vret);
			r = (int32_t)0;
		}
		else {
			r = (int32_t)Int_val(vret);
		}
	);
	CAMLreturnT(int32_t, r);
}
