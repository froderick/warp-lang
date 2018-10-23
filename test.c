#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <execinfo.h>
#include <check.h>
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

void printTokens(Tokens* l) {
  for (unsigned long i=0; i < l->used; i++) {
    Token *t = l->data[i];
    printf("token: '%ls' (%s) %lu %lu\n", t->text, tokenName(t->type), t->position, t->length);
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

  const char * tmpFile = "/tmp/tmp.txt";
  spit(tmpFile, L"(one :two 345 '\"six\") true false nil");

  FILE *stream = fopen(tmpFile, "r");
  if (stream == NULL) {
    handle_error("fopen");
  }

  int err;
  Tokens *tokens = tokensRead(stream, &err);
  fclose(stream);

  //printTokens(tokens);

  ck_assert_int_eq(tokens->used, 10);
  assertToken(tokens->data[0], T_OPAREN,  L"(",      1, 1);
  assertToken(tokens->data[1], T_SYMBOL,  L"one",    4, 3);
  assertToken(tokens->data[2], T_KEYWORD, L"two",    9, 3);
  assertToken(tokens->data[3], T_NUMBER,  L"345",   13, 3);
  assertToken(tokens->data[4], T_QUOTE,   L"'",     15, 1);
  assertToken(tokens->data[5], T_STRING,  L"six",   20, 3);
  assertToken(tokens->data[6], T_CPAREN,  L")",     21, 1);
  assertToken(tokens->data[7], T_TRUE,    L"true",  26, 4);
  assertToken(tokens->data[8], T_FALSE,   L"false", 32, 5);
  assertToken(tokens->data[9], T_NIL,     L"nil",   36, 3);

    tokensFree(tokens);
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
