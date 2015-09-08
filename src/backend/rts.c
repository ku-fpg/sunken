#include <stddef.h>

typedef struct Closure {
  void** free_vars;
  size_t num_free_vars;
  size_t num_args;
  void* (*call)(struct Closure* self, void** args);
} Closure_t;

void* call_closure(Closure_t* cl, void** args) {
  return cl->call(cl, args);
}

// Make a "trivial" closure out of a C function pointer
Closure_t fn_ptr_to_closure(void* (*f)(Closure_t*, void**), size_t num_args) {
  Closure_t cl;
  cl.free_vars = NULL;
  cl.num_free_vars = 0;

  cl.num_args = num_args;
  cl.call = f;
  return cl;
}

