#if !defined ___KCDBCALLBACK_H___
#define ___KCDBCALLBACK_H___

typedef struct {
	value * vfuncfull;
	value * vfuncempty;
	value * vopt;
} CBSTRCT;

extern const char* visitfull_func(const char*, size_t, const char*, size_t, size_t *, void*); 
extern const char* visitfull_string_func(const char*, size_t, const char*, size_t, size_t *, void*); 
extern const char* visitempty_func(const char*, size_t, size_t*, void*);
extern const char* visitempty_string_func(const char*, size_t, size_t*, void*);

extern int32_t fileproc_func(const char*, int64_t, int64_t, void*);
#endif
