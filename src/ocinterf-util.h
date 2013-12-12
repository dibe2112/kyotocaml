#if !defined ___OCINTERF_UTIL_H___
#define ___OCINTERF_UTIL_H___

#include <caml/mlvalues.h>

#define Val_none Val_int(0)
extern CAMLprim value Val_some(value);
#define Some_val(BLOCK) Field(BLOCK,0)

#define RET_UNIT  CAMLreturn (Val_unit)
#define RET_BOOL(I)  CAMLreturn(Val_bool((I) != 0))
#define RET_TRUE	CAMLreturn( Val_true )
#define RET_FALSE	CAMLreturn( Val_false )
#define RET_INT(I)  CAMLreturn(Val_int(I))
#define RET_LONG(L)  CAMLreturn(caml_copy_int64(L))
#define RET_FLOAT(F) CAMLreturn(caml_copy_double(F))
#define RET_OPT(COND,RVAL) CAMLreturn( (COND) ? Val_some(RVAL) : Val_none)
#define RET_NONE CAMLreturn( Val_none )
#define RET_SOME(RVAL) CAMLreturn( Val_some(RVAL) )

#define COPY_STRING_0(P) ( (P) ? caml_copy_string(P) : caml_copy_string("") )
extern CAMLprim value copy_buffer_to_string(const char *, size_t);

extern CAMLprim value mkTpl2(value, value);
extern CAMLprim value mkTpl3(value, value, value);
extern CAMLprim value mkTpl4(value, value, value, value);
extern CAMLprim value mkTpl5(value, value, value, value, value);

extern int length_of_list(value);

/*
#define BLCK_SECT(A) caml_enter_blocking_section(); A; caml_leave_blocking_section() 
#define UNBLCK_SECT(A) caml_leave_blocking_section(); A; caml_enter_blocking_section() 
*/

#include <caml/threads.h>

#define RXA(A) caml_release_runtime_system(); A; caml_acquire_runtime_system() 
#define AXR(A) caml_acquire_runtime_system(); A; caml_release_runtime_system() 

#endif
