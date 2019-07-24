#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include "runtime.h"

/*
 * 1. store main $rsp in global var
 * 2. allocate runtime
 */

int main(void) {


  void* base_pointer;
  asm volatile ("movq %%rbp, %0" : "=r" (base_pointer));

  Runtime rt = rt_create(base_pointer, 1024 * 1024);
  Context ctx = rt_create_parent_frame_context();

  for (int i=0; i<1000; i++) {
      //printf("hi mom\n");
      Value str = rt_alloc_str(rt, ctx, "zomg really long string");
  }


  //printf("I'm alive: %i, %llu\n", rt_val_type(nil) , rt_val_size(nil));
}

// 
//   printf("I'm alive: %i, %llu, %i\n", 
//           rt_val_type(onelist), rt_val_size(onelist), 
//           rt_val_type(rt_val_first(onelist)));
// 
//   Value strB = rt_alloc_str(rt, ctx, "work dammit");
// 
//   rt_println(strB);
// 
//   Value abool = rt_alloc_bool(rt, ctx, true);
//   printf("bool: %i, %llu\n", rt_val_type(abool), rt_val_size(abool));
// 
//   Value bbool = rt_alloc_bool(rt, ctx, false);
//   printf("bools equal: %s\n", rt_val_equals(abool, bbool)  ? "true" : "false");
// 
//   Value aint = rt_alloc_int(rt, ctx, 123);
//   Value bint = rt_alloc_int(rt, ctx, 123);
//   Value zeroint = rt_alloc_int(rt, ctx, 0);
//   printf("ints equal: %s\n", rt_val_equals(aint, bint)  ? "true" : "false");
// 
//   Value astr = rt_alloc_str(rt, ctx, "hooch");
//   Value bstr = rt_alloc_str(rt, ctx, "turner");
//   Value emptystr = rt_alloc_str(rt, ctx, "");
//   printf("strs equal: %s\n", rt_val_equals(astr, bstr)  ? "true" : "false");
// 
//   Value alist = rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "three"),
//     rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "two"),
//       rt_alloc_list(rt, ctx)));
// 
//   Value blist = rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "three"),
//     rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "two"),
//       rt_alloc_list(rt, ctx)));
// 
//   printf("list equal: %s\n", rt_val_equals(alist, blist)  ? "true" : "false");
// 
//   printf("nil truthy?: %s\n", rt_val_truthy(nil)  ? "true" : "false");
//   printf("bool truthy?: %s\n", rt_val_truthy(abool)  ? "true" : "false");
//   printf("bool truthy?: %s\n", rt_val_truthy(bbool)  ? "true" : "false");
//   printf("int truthy?: %s\n", rt_val_truthy(aint)  ? "true" : "false");
//   printf("int zero truthy?: %s\n", rt_val_truthy(zeroint)  ? "true" : "false");
//   printf("str truthy?: %s\n", rt_val_truthy(astr)  ? "true" : "false");
//   printf("str empty truthy?: %s\n", rt_val_truthy(emptystr)  ? "true" : "false");
//   printf("cons truthy?: %s\n", rt_val_truthy(alist)  ? "true" : "false");
//   printf("empty cons truthy?: %s\n", rt_val_truthy(emptylist)  ? "true" : "false");
// 
//   rt_destroy(rt);
// }

//int main(void) {
//
//  void* base_pointer;
//  void* stack_pointer;
//
//  asm volatile ("movq %%rbp, %0" : "=r" (base_pointer));
//  asm volatile ("movq %%rsp, %0" : "=r" (stack_pointer));
//
//  printf("bsp: %p, rsp: %p\n", base_pointer, stack_pointer);
//}
