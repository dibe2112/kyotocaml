#ifndef ___KCCUR_H___
#define ___KCCUR_H___

typedef struct _KCCur {
   KCCUR * p;
} KCCur;

extern CAMLprim value copy_kccur(const KCCur *);

#endif
