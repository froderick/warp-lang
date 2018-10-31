#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <execinfo.h>
#include <check.h>
#include <errno.h>
#include "lexer.h"

void spit(const char* file, const wchar_t* text) {
  FILE *f = fopen(file, "w");

  if (f == NULL) {
    if (DEBUG) { printf("error: failed to open file for writing -> '%s'\n", file); }
    return;
  }

  if (fprintf(f, "%ls", text) < 0) {
    if (DEBUG) { printf("error: failed write to file -> '%s'\n", file); }
  }

  if (fclose(f)) {
    if (DEBUG) { printf("error: failed write to file -> '%s'\n", file); }
  }
}

void assertToken(Token *t,
                 TokenType type, wchar_t *text, unsigned long position, unsigned long length) {
  ck_assert_int_eq(t->type, type);
  ck_assert_msg(wcscmp(t->text, text) == 0, "text must match");
  ck_assert_int_eq(t->position, position);
  ck_assert_int_eq(t->length, length);
}

START_TEST(basic) {

  char * tmpFile = "/tmp/tmp.txt";
  spit(tmpFile, L"(one :two 345 '\"six\") true false nil");

  TokenStream_t stream;
  ck_assert_int_eq(tryStreamMakeFile(tmpFile, &stream), LEX_SUCCESS);

  Token *t;

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_OPAREN, L"(", 1, 1);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_SYMBOL,  L"one",    4, 3);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_KEYWORD, L"two",    9, 3);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_NUMBER,  L"345",   13, 3);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_QUOTE,   L"'",     15, 1);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_STRING,  L"six",   20, 3);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_CPAREN,  L")",     21, 1);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_TRUE,    L"true",  26, 4);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_SUCCESS);
  assertToken(t, T_FALSE,   L"false", 32, 5);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t), LEX_EOF);
  assertToken(t, T_NIL,     L"nil",   36, 3);
  free(t);

  ck_assert_int_eq(tryStreamFree(stream), LEX_SUCCESS);
}
END_TEST

Suite * suite(void) {

    TCase *tc_core = tcase_create("Core");
    tcase_add_test(tc_core, basic);

    Suite *s = suite_create("lexer");
    suite_add_tcase(s, tc_core);

    return s;
}

int main(int argc, char** argv)
{
  int number_failed;
  Suite *s;
  SRunner *sr;

  s = suite();
  sr = srunner_create(s);

  srunner_run_all(sr, CK_NORMAL);
  number_failed = srunner_ntests_failed(sr);
  srunner_free(sr);
  return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
