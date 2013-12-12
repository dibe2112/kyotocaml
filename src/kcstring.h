#if !defined ___KCSTRING_H___
#define ___KCSTRING_H___

#include <kclangc.h>

/*
typedef struct KCSTRING {
   char * buf;
   size_t size;
} KCString;
*/

typedef KCSTR KCString;

#define KCString_val(v) ((KCString *) Data_custom_val(v))
#define KCString_val_buf(v) (((KCString *) Data_custom_val(v))->buf)
#define KCString_val_size(v) (((KCString *) Data_custom_val(v))->size)

extern CAMLprim value copy_kcstring(const KCString *);
extern CAMLprim value copy_kcstring_nofree(const KCString *);
extern CAMLprim value copy_buffer_to_kcstring(const char*, size_t);
extern CAMLprim value copy_buffer_to_kcstring_nofree(const char*, size_t);

extern void free_buf(value);
#endif
