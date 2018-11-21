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

  wchar_t* text = L"(one :two 345 '\"six\") true false nil [] {}";

  Error e;

  StreamSource_t source;
  ck_assert_int_eq(trySourceMakeString(text, wcslen(text), &source, &e), R_SUCCESS);

  TokenStream_t stream;
  ck_assert_int_eq(tryStreamMake(source, &stream, &e), R_SUCCESS);

  Token *t;

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_OPAREN,  L"(",      0, 1);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_SYMBOL,  L"one",    1, 3);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_KEYWORD, L":two",    5, 4);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_NUMBER,  L"345",   10, 3);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_QUOTE,   L"'",     14, 1);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_STRING,  L"\"six\"",   15, 5);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_CPAREN,  L")",     20, 1);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_TRUE,    L"true",  22, 4);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_FALSE,   L"false", 27, 5);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
  assertToken(t, T_NIL,     L"nil",   33, 3);
  free(t);

//  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
//  assertToken(t, T_OVEC,     L"[",   37, 1);
//  free(t);
//
//  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
//  assertToken(t, T_CVEC,     L"]",   38, 1);
//  free(t);
//
//  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
//  assertToken(t, T_OBRACKET, L"{",   40, 1);
//  free(t);
//
//  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
//  assertToken(t, T_CBRACKET, L"}",   41, 1);
  free(t);

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_EOF);
  ck_assert_msg(t == NULL, "when no token is allocated, this pointer should be set to null");

  ck_assert_int_eq(tryStreamFree(stream, &e), R_SUCCESS);
}
END_TEST

START_TEST(eof_mid_number_token) {

    wchar_t* text = L"12345";

    Error e;

    StreamSource_t source;
    ck_assert_int_eq(trySourceMakeString(text, wcslen(text), &source, &e), R_SUCCESS);

    TokenStream_t stream;
    ck_assert_int_eq(tryStreamMake(source, &stream, &e), R_SUCCESS);

    Token *t;

    ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_EOF);
    ck_assert_msg(t != NULL, "when a valid number token is allocated, this pointer should be valid even if an EOF was encountered");
    assertToken(t, T_NUMBER, L"12345", 0, 5);
    free(t);

    ck_assert_int_eq(tryStreamFree(stream, &e), R_SUCCESS);
  }
END_TEST

START_TEST(errors) {

    wchar_t* text = L":";

    Error e;

    StreamSource_t source;
    ck_assert_int_eq(trySourceMakeString(text, wcslen(text), &source, &e), R_SUCCESS);

    TokenStream_t stream;
    ck_assert_int_eq(tryStreamMake(source, &stream, &e), R_SUCCESS);

    Token *t;

    ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_ERROR);
    ck_assert_msg(e.type == E_LEXER);
    ck_assert_msg(e.lexer.position == 0);
    printf("%ls", e.message);

    wchar_t *msg = L"failed to tokenize stream -> keyword token type cannot be empty\n";
    ck_assert_msg(wcscmp(e.message, msg) == 0, "text must match");

    ck_assert_int_eq(tryStreamFree(stream, &e), R_SUCCESS);
  }
END_TEST

Suite * suite(void) {

  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, basic);
  tcase_add_test(tc_core, eof_mid_number_token);
  tcase_add_test(tc_core, errors);

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
