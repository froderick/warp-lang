#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <execinfo.h>
#include <check.h>
#include <errno.h>
#include "errors.h"
#include "lexer.h"

#define handle_error(msg) \
  do { \
    perror(msg); \
    \
    void* callstack[128]; \
    int i, frames = backtrace(callstack, 128); \
    char** strs = backtrace_symbols(callstack, frames); \
    for (i = 0; i < frames; ++i) { \
      printf("%s\n", strs[i]); \
    } \
    free(strs); \
    \
    exit(EXIT_FAILURE); \
  } while (0)

void spit(const char* file, const wchar_t* text) {
  FILE *f = fopen(file, "w");
  if (f == NULL) {
    handle_error("fopen");
  }
  fprintf(f, "%ls", text);
  int err = fclose(f);
  if (err) {
      handle_error("fclose");
  }
}

void assertToken(Token *t,
                 TokenType type, wchar_t *text, unsigned long position, unsigned long length) {
  ck_assert_int_eq(t->type, type);
  ck_assert_msg(wcscmp(t->text, text) == 0, "text must match");
  ck_assert_int_eq(t->position, position);
  ck_assert_int_eq(t->length, length);
}

#define SYSTEM "lexer-test"

void printStacktrace() {
  void* callstack[128];
  int i, frames = backtrace(callstack, 128);
  char** strs = backtrace_symbols(callstack, frames);
  for (i = 0; i < frames; ++i) {
    printf("%s\n", strs[i]);
  }
  free(strs);
}

START_TEST(basic) {

  char * tmpFile = "/tmp/tmp.txt";
  spit(tmpFile, L"(one :two 345 '\"six\") true false nil");

//  FILE *stream = fopen(tmpFile, "r");
//  if (stream == NULL) {
//    handle_error("fopen");
//  }
//
//  Tokens *tokens;
//
//  bool error = tryTokensRead(stream, &tokens);
//  if (error) {
//    printErrors();
//    ck_assert_msg(!error, "lexer encountered errors");
//  }
//
//  if (fclose(stream) != 0) {
//    reportErrnoError(SYSTEM, "errno-error");
//    printErrors();
//    printStacktrace();
//    exit(-1);
//  }
  //printTokens(tokens);
  // TODO: have I screwed up the pointer to the token array, or is the below syntax really needed?


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
