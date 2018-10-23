#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <check.h>
#include "runtime.h"

// https://raw.githubusercontent.com/libcheck/check/master/doc/example/tests/check_money.c

Runtime rt;
Context ctx;

void setup(void) { 

   // TODO: none of this works, shouldn't be doing it here
   void* base_pointer;
   asm volatile ("movq %%rbp, %0" : "=r" (base_pointer));
   ctx = rt_create_parent_frame_context();

   rt = rt_create(base_pointer, 1024 * 1024);
}

void teardown(void) { 
    rt_destroy(rt);
}

START_TEST(test_runtime_basic)
{
  printf("it ran\n");

  Value nil = rt_alloc_nil(rt, ctx);
  Value btrue = rt_alloc_bool(rt, ctx, true);
  Value bfalse = rt_alloc_bool(rt, ctx, false);
  Value emptystr = rt_alloc_str(rt, ctx, "");
  Value str = rt_alloc_str(rt, ctx, "zomg really long string");
  Value anint = rt_alloc_int(rt, ctx, 102);
  Value zeroint = rt_alloc_int(rt, ctx, 0);
  Value emptylist = rt_alloc_list(rt, ctx);
  Value onelist = rt_val_cons(rt, ctx, str, emptylist);

  ck_assert_int_eq(T_NIL, rt_val_type(nil));
  ck_assert_int_eq(T_BOOL, rt_val_type(btrue));
  ck_assert_int_eq(T_STR, rt_val_type(str));
  ck_assert_int_eq(T_INT, rt_val_type(anint));
  ck_assert_int_eq(T_LIST, rt_val_type(emptylist));

  // TODO: why are these so big? (40 bytes for an s_Value??)
  ck_assert_int_eq(40, rt_val_size(nil));
  ck_assert_int_eq(41, rt_val_size(btrue));
  ck_assert_int_eq(64, rt_val_size(str));
  ck_assert_int_eq(48, rt_val_size(anint));
  ck_assert_int_eq(48, rt_val_size(emptylist));

  ck_assert_int_eq(T_STR, rt_val_type(rt_val_first(onelist)));

  rt_println(str); // TODO: how to test this

  ck_assert(rt_val_equals(btrue, btrue));
  ck_assert(!rt_val_equals(btrue, bfalse));

  ck_assert(rt_val_equals(anint, anint));
  ck_assert(!rt_val_equals(anint, zeroint));

  ck_assert(rt_val_equals(str, str));
  ck_assert(!rt_val_equals(str, emptystr));
 
  Value alist = rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "two"),
    rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "one"),
      rt_alloc_list(rt, ctx)));

  Value blist = rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "three"),
    rt_val_cons(rt, ctx, rt_alloc_str(rt, ctx, "one"),
      rt_alloc_list(rt, ctx)));

  ck_assert(rt_val_equals(alist, alist));
  ck_assert(!rt_val_equals(alist, blist));

  ck_assert(!rt_val_truthy(nil));
  ck_assert( rt_val_truthy(btrue));
  ck_assert(!rt_val_truthy(bfalse));
  ck_assert( rt_val_truthy(anint));
  ck_assert( rt_val_truthy(str));
  ck_assert( rt_val_truthy(emptystr));
  ck_assert( rt_val_truthy(onelist));
  ck_assert( rt_val_truthy(emptylist));

  rt_val_plus(rt, ctx, anint, anint);

  Value fnval = rt_alloc_fn(rt, ctx, (void*)100);
  ck_assert_ptr_eq((void*)100, rt_val_fn_ptr(fnval));
}
END_TEST

Suite* money_suite(void)
{
    Suite* s = suite_create("runtime");
    TCase* tc_core = tcase_create("core");

    /* Core test case */
    tcase_add_checked_fixture(tc_core, setup, teardown);
    tcase_add_test(tc_core, test_runtime_basic);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite* s;
    SRunner* sr;

    s = money_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}


