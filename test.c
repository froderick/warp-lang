#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <execinfo.h>
#include <check.h>
#include <errno.h>
#include "reader.h"
#include "analyzer.h"
#include "vm.h"

void assertToken(Token *t,
                 TokenType type, wchar_t *text, unsigned long position, unsigned long length) {

  ck_assert_int_eq(t->type, type);
  ck_assert_msg(wcscmp(t->text, text) == 0, "text must match");
  ck_assert_int_eq(t->source.position, position);
  ck_assert_int_eq(t->source.length, length);
}

START_TEST(basic) {

  wchar_t* text = L"(one :two 345 '\"six\") true false nil";

  Error e;

  InputStream_t source;
  ck_assert_int_eq(tryStringInputStreamMake(text, wcslen(text), &source, &e), R_SUCCESS);

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

  ck_assert_int_eq(tryStreamNext(stream, &t, &e), R_EOF);
  ck_assert_msg(t == NULL, "when no token is allocated, this pointer should be set to null");

  ck_assert_int_eq(tryStreamFree(stream, &e), R_SUCCESS);
}
END_TEST

START_TEST(eof_mid_number_token) {

    wchar_t* text = L"12345";

    Error e;

    InputStream_t source;
    ck_assert_int_eq(tryStringInputStreamMake(text, wcslen(text), &source, &e), R_SUCCESS);

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

    InputStream_t source;
    ck_assert_int_eq(tryStringInputStreamMake(text, wcslen(text), &source, &e), R_SUCCESS);

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

    wchar_t* input = L"\"str\" 102 \n himom :rocks true nil (true false) 'nil";

    Error e;
    InputStream_t source;
    TokenStream_t stream;
    Expr *expr;

    ck_assert_int_eq(tryStringInputStreamMake(input, wcslen(input), &source, &e), R_SUCCESS);
    ck_assert_int_eq(tryStreamMake(source, &stream, &e), R_SUCCESS);

    // string
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_STRING);
    ck_assert(wcscmp(expr->string.value, L"str") == 0);
    ck_assert_int_eq(expr->string.length, 3);
    ck_assert_int_eq(expr->source.lineNumber, 1);
    ck_assert_int_eq(expr->source.colNumber, 1);
    exprFree(expr);

    // number
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_NUMBER);
    ck_assert_int_eq(expr->number.value, 102);
    ck_assert_int_eq(expr->source.lineNumber, 1);
    ck_assert_int_eq(expr->source.colNumber, 6);
    exprFree(expr);

    // symbol
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_SYMBOL);
    ck_assert(wcscmp(expr->symbol.value, L"himom") == 0);
    ck_assert_int_eq(expr->symbol.length, 5);
    ck_assert_int_eq(expr->source.lineNumber, 2);
    ck_assert_int_eq(expr->source.colNumber, 1);
    exprFree(expr);

    // keyword
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_KEYWORD);
    ck_assert(wcscmp(expr->keyword.value, L"rocks") == 0);
    ck_assert_int_eq(expr->keyword.length, 5);
    exprFree(expr);

    // boolean
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_BOOLEAN);
    ck_assert(expr->boolean.value == true);
    exprFree(expr);

    // nil
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_NIL);
    exprFree(expr);

    // list
    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == N_LIST);
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

    ck_assert_int_eq(tryExprRead(stream, &expr, &e), R_EOF);
  }
END_TEST

RetVal tryParse(wchar_t *input, Expr **ptr, Error *error) {

  RetVal ret;

  InputStream_t source;
  TokenStream_t stream;
  Expr *expr;

  throws(tryStringInputStreamMake(input, wcslen(input), &source, error));
  throws(tryStreamMake(source, &stream, error));

  throws(tryExprRead(stream, &expr, error));

  throws(tryStreamFree(stream, error));

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

START_TEST(exprPrn)
  {

    Error e;
    FormAnalyzer *analyzer;
    Expr *expr;

    ck_assert_int_eq(tryAnalyzerMake(&analyzer, &e), R_SUCCESS);

    // constant
    ck_assert_int_eq(tryParse(L"(himom () '(one :two 102 nil true false) \"str\")", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryExprPrn(expr, stdin, &e), R_SUCCESS);
    printf("\n");
    exprFree(expr);
  }
END_TEST

START_TEST(analyzer) {

    Error e;
    FormAnalyzer *analyzer;
    Expr *expr;
    Form *form;

    ck_assert_int_eq(tryAnalyzerMake(&analyzer, &e), R_SUCCESS);

    // constant
    ck_assert_int_eq(tryParse(L"\"str\"", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_CONST);
    formFree(form);

    // if
    ck_assert_int_eq(tryParse(L"(if true 10 20)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_IF);
    formFree(form);

    // let
    ck_assert_int_eq(tryParse(L"(let (a nil) true)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_LET);
    formFree(form);

    // env-ref
    ck_assert_int_eq(tryParse(L"(let (a nil) a)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_LET);
    ck_assert_int_eq(form->let.forms[0].type, F_ENV_REF);
    ck_assert_int_eq(form->let.forms[0].envRef.index, 0);
    ck_assert_int_eq(form->let.forms[0].envRef.type, RT_LOCAL);
    formFree(form);

    // def
    ck_assert_int_eq(tryParse(L"(def money 100)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_DEF);
    formFree(form);

    // var-ref
    ck_assert_int_eq(tryParse(L"money", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_VAR_REF);
    formFree(form);

    // fn with args
    ck_assert_int_eq(tryParse(L"(fn (a b c) a)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_FN);
    ck_assert_int_eq(form->fn.forms[0].type, F_ENV_REF);
    ck_assert_int_eq(form->fn.forms[0].envRef.index, 0);
    ck_assert_int_eq(form->fn.forms[0].envRef.type, RT_ARG);
    formFree(form);

    // fn-call
    ck_assert_int_eq(tryParse(L"(def barf (fn () 100))", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_DEF);
    formFree(form);

    ck_assert_int_eq(tryParse(L"(barf)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(analyzer, expr, &form, &e), R_SUCCESS);
    exprFree(expr);
    ck_assert_int_eq(form->type, F_FN_CALL);
    ck_assert_int_eq(form->fnCall.fnCallable->type , F_VAR_REF);
    ck_assert_int_eq(form->fnCall.fnCallable->varRef.var->value->type, F_FN);
    formFree(form);

// TODO:
//        F_BUILTIN,

  }
END_TEST

START_TEST(vmBasic) {

    Error error;
    VM_t vm;
    Value result;

    ck_assert_int_eq(tryVMMake(&vm, &error), R_SUCCESS);

    CodeUnit unit;

    unit.numConstants = 2;
    unit.constants = malloc(sizeof(Constant) * unit.numConstants);
    unit.constants[0].type = CT_INT;
    unit.constants[0].integer = 100;
    unit.constants[1].type = CT_INT;
    unit.constants[1].integer = 2;

    unit.numFunctionDefinitions = 0;
    unit.functionDefinitions = NULL;
    unit.code.numLocals = 0;
    unit.code.maxOperandStackSize = 10;

    uint8_t code[] = {
        I_LOAD_CONST, 0, 0,
        I_LOAD_CONST, 0, 1,
        I_PLUS,
        I_RET
    };
    unit.code.codeLength = sizeof(code);
    unit.code.code = code;

    ck_assert_int_eq(tryVMEval(vm, &unit, &result, &error), R_SUCCESS);
    ck_assert_int_eq(result.type, VT_UINT);
    ck_assert_int_eq(result.value, 102);

    vmFree(vm);
  }
END_TEST

Suite * suite(void) {

  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, basic);
  tcase_add_test(tc_core, eof_mid_number_token);
  tcase_add_test(tc_core, errors);
  tcase_add_test(tc_core, parser);
  tcase_add_test(tc_core, exprPrn);
  tcase_add_test(tc_core, analyzer);
  tcase_add_test(tc_core, vmBasic);

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
