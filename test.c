#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <execinfo.h>
#include <check.h>
#include <errno.h>
#include "reader.h"

void assertToken(Token *t,
                 TokenType type, wchar_t *text, unsigned long position, unsigned long length) {

  ck_assert_int_eq(t->type, type);
  ck_assert_msg(wcscmp(t->text, text) == 0, "text must match");
  ck_assert_int_eq(t->position, position);
  ck_assert_int_eq(t->length, length);
}

START_TEST(basic) {

  wchar_t* text = L"(one :two 345 '\"six\") true false nil";

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
//  free(t);

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
    ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_SUCCESS);
    assertToken(t, T_NUMBER, L"12345", 0, 5);
    tokenFree(t);

    ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_EOF);
    ck_assert_msg(t == NULL, "no tokens remain on the stream");
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
//    printf("%ls", e.message);

    wchar_t *msg = L"failed to tokenize stream -> keyword token type cannot be empty\n";
    ck_assert_msg(wcscmp(e.message, msg) == 0, "text must match");

    ck_assert_int_eq(tryStreamFree(stream, &e), R_SUCCESS);
  }
END_TEST

START_TEST(parser) {

    wchar_t* input = L"\"str\" 102 himom :rocks true nil (true false) 'nil";

    Error e;
    StreamSource_t source;
    TokenStream_t stream;
    Expr *expr;

    ck_assert_int_eq(trySourceMakeString(input, wcslen(input), &source, &e), R_SUCCESS);
    ck_assert_int_eq(tryStreamMake(source, &stream, &e), R_SUCCESS);

    // string
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_STRING);
    ck_assert(wcscmp(expr->string.value, L"str") == 0);
    ck_assert(expr->string.token->type == T_STRING);
    exprFree(expr);

    // number
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_NUMBER);
    ck_assert_int_eq(expr->number.value, 102);
    ck_assert(expr->number.token->type == T_NUMBER);
    exprFree(expr);

    // symbol
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_SYMBOL);
    ck_assert(wcscmp(expr->symbol.value, L"himom") == 0);
    ck_assert(expr->symbol.token->type == T_SYMBOL);
    exprFree(expr);

    // keyword
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_KEYWORD);
    ck_assert(wcscmp(expr->keyword.value, L"rocks") == 0);
    ck_assert(expr->keyword.token->type == T_KEYWORD);
    exprFree(expr);

    // boolean
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_BOOLEAN);
    ck_assert(expr->boolean.value == true);
    ck_assert(expr->boolean.token->type == T_TRUE);
    exprFree(expr);

    // nil
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_NIL);
    ck_assert(expr->nil.token->type == T_NIL);
    exprFree(expr);

    // list
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_LIST);
    ck_assert(expr->list.oParen->type == T_OPAREN);
    ck_assert(expr->list.cParen->type == T_CPAREN);
    ck_assert(expr->list.length == 2);
    // first element
    ck_assert(expr->list.head->expr->type == N_BOOLEAN);
    ck_assert(expr->list.head->expr->boolean.value == true);
    // second element
    ck_assert(expr->list.head->next->expr->type == N_BOOLEAN);
    ck_assert(expr->list.head->next->expr->boolean.value == false);
    // verify second element is tail
    ck_assert(expr->list.tail == expr->list.head->next);
    exprFree(expr);

    // quote (reader macro)
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_LIST);
    ck_assert(expr->list.length == 2);
    // first element
    ck_assert(expr->list.head->expr->type == N_SYMBOL);
    ck_assert(wcscmp(expr->list.head->expr->symbol.value, L"quote") == 0);
    // second element
    ck_assert(expr->list.head->next->expr->type == N_NIL);
    exprFree(expr);
  }
END_TEST

Suite * suite(void) {

  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, basic);
  tcase_add_test(tc_core, eof_mid_number_token);
  tcase_add_test(tc_core, errors);
  tcase_add_test(tc_core, parser);

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
