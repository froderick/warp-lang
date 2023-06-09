#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <execinfo.h>
#include <check.h>
#include <errno.h>
#include "bootstrap/reader.h"
#include "bootstrap/analyzer.h"
#include "bootstrap/compiler.h"
#include "vm/vm.h"
#include "bootstrap/repl.h"
#include "bootstrap/print.h"

#define ONE_MB (1024 * 1000)

VMConfig config;

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
  errorInitContents(&e);

  Pool_t pool = poolCreate(ONE_MB);

  InputStream_t source;
  ck_assert_int_eq(tryStringInputStreamMake(pool, text, wcslen(text), &source, &e), R_SUCCESS);

  TokenStream_t stream = streamMake(pool, source);

  Token *t;

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_OPAREN,  L"(",      0, 1);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_SYMBOL,  L"one",    1, 3);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_KEYWORD, L":two",    5, 4);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_NUMBER,  L"345",   10, 3);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_QUOTE,   L"'",     14, 1);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_STRING,  L"\"six\"",   15, 5);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_CPAREN,  L")",     20, 1);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_TRUE,    L"true",  22, 4);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_FALSE,   L"false", 27, 5);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
  assertToken(t, T_NIL,     L"nil",   33, 3);

  ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_EOF);
  ck_assert_msg(t == NULL, "when no token is allocated, this pointer should be set to null");

  ck_assert_int_eq(tryInputStreamFree(source, &e), R_SUCCESS);
}
END_TEST

START_TEST(eof_mid_number_token) {

    wchar_t* text = L"12345";

    Error e;
    errorInitContents(&e);

    Pool_t pool = poolCreate(ONE_MB);

    InputStream_t source;
    ck_assert_int_eq(tryStringInputStreamMake(pool, text, wcslen(text), &source, &e), R_SUCCESS);

    TokenStream_t stream = streamMake(pool, source);

    Token *t;
    ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_SUCCESS);
    assertToken(t, T_NUMBER, L"12345", 0, 5);

    ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_EOF);
    ck_assert_msg(t == NULL, "no tokens remain on the stream");

    ck_assert_int_eq(tryInputStreamFree(source, &e), R_SUCCESS);

    poolFree(pool);
  }
END_TEST

START_TEST(errors) {

    wchar_t* text = L":";

    Error e;
    errorInitContents(&e);

    Pool_t pool = poolCreate(ONE_MB);

    InputStream_t source;
    ck_assert_int_eq(tryStringInputStreamMake(pool, text, wcslen(text), &source, &e), R_SUCCESS);

    TokenStream_t stream = streamMake(pool, source);

    Token *t;

    ck_assert_int_eq(tryStreamNext(pool, stream, &t, &e), R_ERROR);
    ck_assert_msg(e.type == E_LEXER, "need a lexer");
    ck_assert_msg(e.lexer.position == 0, "need position zero");
//    printf("%ls", e.message);

    wchar_t *msg = L"failed to tokenize stream -> keyword token type cannot be empty\n";
    ck_assert_msg(wcscmp(e.message, msg) == 0, "text must match");

    ck_assert_int_eq(tryInputStreamFree(source, &e), R_SUCCESS);

    poolFree(pool);
  }
END_TEST

START_TEST(parser) {

    wchar_t* input = L"\"str\" 102 \n himom :rocks true nil (true false) 'nil 'X'";

    Error e;
    errorInitContents(&e);
    InputStream_t source;
    Form *expr;

    Pool_t pool = poolCreate(ONE_MB);

    ck_assert_int_eq(tryStringInputStreamMake(pool, input, wcslen(input), &source, &e), R_SUCCESS);
    TokenStream_t stream = streamMake(pool, source);

    // string
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_STRING);
    ck_assert(wcscmp(expr->string.value, L"str") == 0);
    ck_assert_int_eq(expr->string.length, 3);
    ck_assert_int_eq(expr->source.lineNumber, 1);
    ck_assert_int_eq(expr->source.colNumber, 1);

    // number
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_NUMBER);
    ck_assert_int_eq(expr->number.value, 102);
    ck_assert_int_eq(expr->source.lineNumber, 1);
    ck_assert_int_eq(expr->source.colNumber, 6);

    // symbol
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_SYMBOL);
    ck_assert(wcscmp(expr->symbol.value, L"himom") == 0);
    ck_assert_int_eq(expr->symbol.length, 5);
    ck_assert_int_eq(expr->source.lineNumber, 2);
    ck_assert_int_eq(expr->source.colNumber, 1);

    // keyword
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_KEYWORD);
    ck_assert(wcscmp(expr->keyword.value, L"rocks") == 0);
    ck_assert_int_eq(expr->keyword.length, 5);

    // boolean
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_BOOLEAN);
    ck_assert(expr->boolean.value == true);

    // nil
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_NIL);

    // list
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_LIST);
    ck_assert(expr->list.length == 2);
    // first element
    ck_assert(expr->list.head->expr->type == F_BOOLEAN);
    ck_assert(expr->list.head->expr->boolean.value == true);
    // second element
    ck_assert(expr->list.head->next->expr->type == F_BOOLEAN);
    ck_assert(expr->list.head->next->expr->boolean.value == false);
    // verify second element is tail
    ck_assert(expr->list.tail == expr->list.head->next);

    // quote (reader macro)
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_LIST);
    ck_assert(expr->list.length == 2);
    // first element
    ck_assert(expr->list.head->expr->type == F_SYMBOL);
    ck_assert(wcscmp(expr->list.head->expr->symbol.value, L"quote") == 0);
    // second element
    ck_assert(expr->list.head->next->expr->type == F_NIL);

    // character
    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_SUCCESS);
    ck_assert(expr->type == F_CHAR);
    ck_assert_int_eq(expr->chr.value, L'X');
    ck_assert_int_eq(expr->source.lineNumber, 2);
    ck_assert_int_eq(expr->source.colNumber, 34);

    ck_assert_int_eq(tryExprRead(pool, stream, &expr, &e), R_EOF);

    poolFree(pool);
  }
END_TEST

RetVal tryParse(Pool_t pool, wchar_t *input, Form **ptr, Error *error) {

  RetVal ret;

  InputStream_t source;
  Form *expr;

  throws(tryStringInputStreamMake(pool, input, wcslen(input), &source, error));
  TokenStream_t stream = streamMake(pool, source);

  throws(tryExprRead(pool, stream, &expr, error));

  throws(tryInputStreamFree(source, error));

  *ptr = expr;
  return R_SUCCESS;

  failure:
    return ret;
}

START_TEST(exprPrnTest)
  {

    Error e;
    Form *expr;
    errorInitContents(&e);

    Pool_t pool = poolCreate(ONE_MB);

    // constant
    ck_assert_int_eq(tryParse(pool, L"(himom () '(one :two 102 nil true false) \"str\")", &expr, &e), R_SUCCESS);
    exprPrn(pool, expr);
    printf("\n");

    poolFree(pool);
  }
END_TEST

START_TEST(analyzer) {

    Error e;
    Form *expr;
    FormRoot *root;

    errorInitContents(&e);
    Pool_t pool = poolCreate(ONE_MB);

    // constant
    ck_assert_int_eq(tryParse(pool, L"\"str\"", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_STRING);

    // if
    ck_assert_int_eq(tryParse(pool, L"(if true 10 20)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_IF);

    // let
    ck_assert_int_eq(tryParse(pool, L"(let* (a nil) true)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_LET);

    // env-ref
    ck_assert_int_eq(tryParse(pool, L"(let* (a nil) a)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_LET);
    ck_assert_int_eq(root->form->let.forms.forms[0].type, F_ENV_REF);
    ck_assert_int_eq(root->form->let.forms.forms[0].envRef.bindingIndex, 0);

    // def
    ck_assert_int_eq(tryParse(pool, L"(def money 100)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_DEF);

    // var-ref
    ck_assert_int_eq(tryParse(pool, L"money", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_VAR_REF);

    // fn with args
    ck_assert_int_eq(tryParse(pool, L"(fn (a b c) a)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_FN);
    ck_assert_int_eq(root->form->fn.forms.forms[0].type, F_ENV_REF);
    ck_assert_int_eq(root->form->fn.forms.forms[0].envRef.bindingIndex, 0);

    // fn-call
    ck_assert_int_eq(tryParse(pool, L"(def barf (fn () 100))", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_DEF);

    ck_assert_int_eq(tryParse(pool, L"(barf)", &expr, &e), R_SUCCESS);
    ck_assert_int_eq(tryFormAnalyze(expr, pool, &root, &e), R_SUCCESS);
    ck_assert_int_eq(root->form->type, F_FN_CALL);
    ck_assert_int_eq(root->form->fnCall.fnCallable->type , F_VAR_REF);

    poolFree(pool);

  }
END_TEST

RetVal tryTestCompile(wchar_t *input, CodeUnit *codeUnit, Error *error) {

  RetVal ret;

  Pool_t pool = poolCreate(ONE_MB);

  Form *expr;
  FormRoot *root;

  throws(tryParse(pool, input, &expr, error));
  throws(tryFormAnalyze(expr, pool, &root, error));
  compileTopLevel(pool, root, codeUnit);

  poolFree(pool);

  return R_SUCCESS;

  failure:
    return ret;
}

START_TEST(compilerBasic) {

    Error e;
    CodeUnit codeUnit;

    errorInitContents(&e);

    // builtin add
    {
      ck_assert_int_eq(tryTestCompile(L"(builtin :add 1 2)", &codeUnit, &e), R_SUCCESS);

      //printCodeUnit(&codeUnit);

      ck_assert_int_eq(codeUnit.numConstants, 2);
      ck_assert_int_eq(codeUnit.constants[0].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[0].integer, 1);
      ck_assert_int_eq(codeUnit.constants[1].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[1].integer, 2);

      ck_assert_int_eq(codeUnit.code.numLocals, 0);
      ck_assert_int_eq(codeUnit.code.maxOperandStackSize, 2);
      ck_assert_int_eq(codeUnit.code.hasSourceTable, false);

      uint8_t expectedCode[] = {
          I_LOAD_CONST, 0, 0,
          I_LOAD_CONST, 0, 1,
          I_ADD,
          I_RET,
      };

      ck_assert_int_eq(codeUnit.code.codeLength, sizeof(expectedCode));
      ck_assert(memcmp(expectedCode, codeUnit.code.code, codeUnit.code.codeLength) == 0);
    }

    // fn and fn-call
    {
      ck_assert_int_eq(tryTestCompile(L"((fn (a b) (builtin :add a b)) 4 5)", &codeUnit, &e), R_SUCCESS);

      //printCodeUnit(&codeUnit);

      // verify fn

      FnConstant fn = codeUnit.constants[2].function;
      ck_assert_int_eq(fn.numArgs, 2);
      ck_assert_int_eq(fn.numConstants, 0);
      ck_assert_int_eq(fn.code.numLocals, 2);
      ck_assert_int_eq(fn.code.maxOperandStackSize, 2);
      ck_assert_int_eq(fn.code.hasSourceTable, false);

      uint8_t fnCode[] = {
          I_LOAD_LOCAL, 0, 0,
          I_LOAD_LOCAL, 0, 1,
          I_ADD,
          I_RET,
      };

      ck_assert_int_eq(fn.code.codeLength, sizeof(fnCode));
      ck_assert(memcmp(fnCode, fn.code.code, fn.code.codeLength) == 0);

      // verify fnCall

      ck_assert_int_eq(codeUnit.numConstants, 3);
      ck_assert_int_eq(codeUnit.constants[0].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[0].integer, 4);
      ck_assert_int_eq(codeUnit.constants[1].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[1].integer, 5);

      ck_assert_int_eq(codeUnit.code.numLocals, 0);
      ck_assert_int_eq(codeUnit.code.maxOperandStackSize, 3);
      ck_assert_int_eq(codeUnit.code.hasSourceTable, false);

      uint8_t fnCallCode[] = {
          I_LOAD_CONST, 0, 0,
          I_LOAD_CONST, 0, 1,
          I_LOAD_CONST, 0, 2,
          I_INVOKE_DYN, 0, 2,
          I_RET,
      };

      ck_assert_int_eq(codeUnit.code.codeLength, sizeof(fnCallCode));
      ck_assert(memcmp(fnCallCode, codeUnit.code.code, codeUnit.code.codeLength) == 0);
    }

    // let
    {
      ck_assert_int_eq(tryTestCompile(L"(let* (x 12) x)", &codeUnit, &e), R_SUCCESS);

      ck_assert_int_eq(codeUnit.numConstants, 1);
      ck_assert_int_eq(codeUnit.constants[0].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[0].integer, 12);

      ck_assert_int_eq(codeUnit.code.numLocals, 1);
      ck_assert_int_eq(codeUnit.code.maxOperandStackSize, 1);
      ck_assert_int_eq(codeUnit.code.hasSourceTable, false);

      uint8_t expectedCode[] = {
          I_LOAD_CONST, 0, 0,
          I_STORE_LOCAL, 0, 0,
          I_LOAD_LOCAL, 0, 0,
          I_RET,
      };

//      printCodeUnit(&codeUnit);
//      printf("-------------\n");
//      printCodeArray(expectedCode, sizeof(expectedCode));

      ck_assert_int_eq(codeUnit.code.codeLength, sizeof(expectedCode));
      ck_assert(memcmp(expectedCode, codeUnit.code.code, codeUnit.code.codeLength) == 0);
    }

    // let (nested)
    {
      ck_assert_int_eq(tryTestCompile(L"(let* (x 12 "
                                       "      y (let* (z 100) z)) "
                                       "  y)", &codeUnit, &e), R_SUCCESS);

      ck_assert_int_eq(codeUnit.numConstants, 2);
      ck_assert_int_eq(codeUnit.constants[0].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[0].integer, 12);
      ck_assert_int_eq(codeUnit.constants[1].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[1].integer, 100);

      ck_assert_int_eq(codeUnit.code.numLocals, 3);
      ck_assert_int_eq(codeUnit.code.maxOperandStackSize, 1);
      ck_assert_int_eq(codeUnit.code.hasSourceTable, false);

      uint8_t expectedCode[] = {
          I_LOAD_CONST,  0, 0,
          I_STORE_LOCAL, 0, 0,
          I_LOAD_CONST,  0, 1,
          I_STORE_LOCAL, 0, 1,
          I_LOAD_LOCAL,  0, 1,
          I_STORE_LOCAL, 0, 2,
          I_LOAD_LOCAL,  0, 2,
          I_RET,
      };

//      printCodeUnit(&codeUnit);
//      printf("-------------\n");
//      printCodeArray(expectedCode, sizeof(expectedCode));

      ck_assert_int_eq(codeUnit.code.codeLength, sizeof(expectedCode));
      ck_assert(memcmp(expectedCode, codeUnit.code.code, codeUnit.code.codeLength) == 0);
    }

    // let fn and call
    {
      ck_assert_int_eq(tryTestCompile(L"(let* (x (fn (y) (builtin :add y 50)))"
                                       "  (x 100))", &codeUnit, &e), R_SUCCESS);

      //printCodeUnit(&codeUnit);

      ck_assert_int_eq(codeUnit.numConstants, 2);
      ck_assert_int_eq(codeUnit.constants[0].type, CT_FN);
      ck_assert_int_eq(codeUnit.constants[1].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[1].integer, 100);

      ck_assert_int_eq(codeUnit.code.numLocals, 1);
      ck_assert_int_eq(codeUnit.code.maxOperandStackSize, 2);
      ck_assert_int_eq(codeUnit.code.hasSourceTable, false);

      uint8_t expectedCode[] = {
          I_LOAD_CONST,  0, 0,
          I_STORE_LOCAL, 0, 0,
          I_LOAD_CONST,  0, 1,
          I_LOAD_LOCAL,  0, 0,
          I_INVOKE_DYN,  0, 1,
          I_RET,
      };

      //printCodeUnit(&codeUnit);
      //printf("-------------\n");
      //printCodeArray(expectedCode, sizeof(expectedCode));

      ck_assert_int_eq(codeUnit.code.codeLength, sizeof(expectedCode));
      ck_assert(memcmp(expectedCode, codeUnit.code.code, codeUnit.code.codeLength) == 0);
    }

    // define, var-ref
    {
      ck_assert_int_eq(tryTestCompile(L"(let* ()"
                                       "  (def x 100)"
                                       "  (builtin :add x 50))", &codeUnit, &e), R_SUCCESS);

      ck_assert_int_eq(codeUnit.numConstants, 3);
      ck_assert_int_eq(codeUnit.constants[0].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[0].integer, 100);
      ck_assert_int_eq(codeUnit.constants[1].type, CT_SYMBOL);
      ck_assert_int_eq(codeUnit.constants[1].symbol.length, 1);
      ck_assert_int_eq(wcscmp(codeUnit.constants[1].symbol.value, L"x"), 0);
      ck_assert_int_eq(codeUnit.constants[2].type, CT_INT);
      ck_assert_int_eq(codeUnit.constants[2].integer, 50);

      ck_assert_int_eq(codeUnit.code.numLocals, 0);
      ck_assert_int_eq(codeUnit.code.maxOperandStackSize, 2);
      ck_assert_int_eq(codeUnit.code.hasSourceTable, false);

      uint8_t expectedCode[] = {
          I_LOAD_CONST,  0, 0,
          I_DEF_VAR,     0, 1,
          I_DROP,
          I_LOAD_VAR,    0, 1,
          I_LOAD_CONST,  0, 2,
          I_ADD,
          I_RET,
      };

//      printCodeUnit(&codeUnit);
//      printf("-------------\n");
//      printCodeArray(expectedCode, sizeof(expectedCode));

      ck_assert_int_eq(codeUnit.code.codeLength, sizeof(expectedCode));
      ck_assert(memcmp(expectedCode, codeUnit.code.code, codeUnit.code.codeLength) == 0);
    }

    // TODO: nested let + rebind in sub-let
  }
END_TEST

START_TEST(vmBasic) {

    Pool_t pool = poolCreate(ONE_MB);
    Error error;
    VM_t vm = NULL;
    VMEvalResult result;

    errorInitContents(&error);
    vm = vmMake(config);

    uint8_t fnCode[] = {
        I_LOAD_CONST, 0, 0,
        I_LOAD_LOCAL, 0, 0,
        I_ADD,
        I_RET
    };

    FnConstant fn;
    constantFnInitContents(&fn);
    fn.numConstants = 1;
    fn.constants = malloc(sizeof(Constant) * fn.numConstants);
    fn.constants[0].type = CT_INT;
    fn.constants[0].integer = 100;
    fn.numArgs = 1;
    fn.code.numLocals = 1;
    fn.code.maxOperandStackSize = 100;
    fn.code.codeLength = sizeof(fnCode);
    fn.code.code = fnCode;
    fn.code.hasSourceTable = false;

    uint8_t code[] = {
        I_LOAD_CONST, 0, 0,
        I_LOAD_CONST, 0, 1,
        I_INVOKE_DYN, 0, 1,
        I_RET
    };

    CodeUnit unit;
    codeUnitInitContents(&unit);
    unit.numConstants = 2;
    unit.constants = malloc(sizeof(Constant) * unit.numConstants);
    unit.constants[0].type = CT_INT;
    unit.constants[0].integer = 10;
    unit.constants[1].type = CT_FN;
    unit.constants[1].function = fn;
    unit.code.numLocals = 0;
    unit.code.maxOperandStackSize = 100;
    unit.code.codeLength = sizeof(code);
    unit.code.code = code;
    unit.code.hasSourceTable = false;

    result = vmEval(vm, &unit);
    ck_assert_int_eq(result.type, RT_RESULT);

    ck_assert_int_eq(valueType(result.value), VT_UINT);
    ck_assert_int_eq(unwrapUint(result.value), 110);

    vmFree(vm);
    poolFree(pool);
  }
END_TEST

wchar_t* testEval(VM_t vm, Pool_t pool, wchar_t *inputText) {

  printf("%ls\n", inputText);

  CodeUnit unit;
  InputStream_t source = NULL;
  TokenStream_t stream = NULL;

  Error error;
  errorInitContents(&error);

  codeUnitInitContents(&unit);
  if (tryStringInputStreamMake(pool, inputText, wcslen(inputText), &source, &error) != R_SUCCESS) {
    explode("failed to make input stream");
  }
  stream = streamMake(pool, source);

  FileInfo fileInfo;
  fileInfoInitContents(&fileInfo);

  if (tryReplCompile(pool, stream, fileInfo, vm, &unit, &error) != R_SUCCESS) {
    explode("failed to compile input");
  }
  printCodeUnit(&unit);

  VMEvalResult result = vmEval(vm, &unit);

  if (result.type == RT_RESULT) {
    Form *expr = printToReader(vm, pool, result.value);
    return exprPrnStr(pool, expr);
  }
  else {
    printException(vm, result.value);
    return NULL;
  }
}

#define assertEval(inputText, expectedOutputText) { \
  wchar_t *result = testEval(vm, pool, inputText); \
  if (result != NULL) { \
    if (wcscmp(expectedOutputText, result) != 0) { \
      ck_abort_msg("got '%ls', expected '%ls'", result, expectedOutputText); \
    } \
  } \
  else { \
    ck_abort_msg("exception thrown"); \
  } \
}

#define assertEvalException(inputText) { \
  wchar_t *result = testEval(vm, pool, inputText); \
  if (result == NULL) { \
  } \
  else { \
    ck_abort_msg("exception not thrown"); \
  } \
}

START_TEST(repl)
  {

    Error error;
    errorInitContents(&error);

    Pool_t pool = poolCreate(ONE_MB * 10);
    VM_t vm = vmMake(config);
    ck_assert_int_eq(tryLoad(vm, STD_LIB, &error), R_SUCCESS);

    assertEval(L"(let ()"
               "  (def + (fn (a b) (builtin :add a b)))"
               "  (+ 1 2))",
               L"3");

    assertEval(L"(if 1 2 3)",
               L"2");

    assertEval(L"(if 0 2 3)",
               L"2");

    assertEval(L"(if 'x 'y)",
               L"y");

    assertEval(L"(if false 'y)",
               L"nil");

    assertEval(L"(let ()"
               "  (def + (fn (a b) (builtin :add a b)))"
               "  (let (a 100) "
               "    (+ a 20)))",
               L"120");

    assertEval(L"(let (x (fn (a b) (builtin :add a b))) (x 1 2))",
               L"3");

    assertEval(L"(builtin :compare 1 2)",
               L"false");

    assertEval(L"(builtin :compare 2 2)",
               L"true");

    assertEval(L"nil",
               L"nil");

    assertEval(L"'hi",
               L"hi");

    assertEval(L":hi",
               L":hi");

    assertEval(L"'(1 2 3)",
               L"(1 2 3)");

    assertEval(L"(first '(x y z))",
               L"x");

    assertEval(L"(first nil)",
               L"nil");

    assertEval(L"(rest '(x y z))",
               L"(y z)");

    assertEval(L"(rest nil)",
               L"nil");

    assertEval(L"(cons nil nil)",
               L"(nil)");

    assertEval(L"(cons 'x nil)",
               L"(x)");

    assertEval(L"(cons 'x '(y z))",
               L"(x y z)");

    assertEval(L"(def x)",
               L"nil");

    assertEval(L"(fn foo () 'x)",
               L"\"<function>\"");

    assertEval(L"(let (x (fn more (n)"
               "           (if (builtin :compare n 5)"
               "             n"
               "             (more (builtin :add n 1)))))"
               "   (x 0))",
               L"5");

    assertEval(L"(builtin :subtract 10 2)", L"8");

    assertEval(L"(let (a 100) (fn () a))", L"\"<closure>\"");

    assertEval(L"(let (a 100 b (fn b () a)) (b))",
               L"100");

    assertEval(L"(let (minimum 101"
               "       adder (fn (n) (builtin :add minimum n))"
               "       doer (fn () (adder 5)))"
               "   (doer))",
               L"106");

    assertEval(L"(let () "
               "    (def foo (fn (x) x))"
               "    (set-macro 'foo))", L"nil");

    assertEval(L"(let () (def x 100) x)", L"100");

    assertEval(L"(list 1 2)", L"(1 2)");

    assertEval(L"`(1 2 ~(builtin :add 3 1))", L"(1 2 4)");

    assertEval(L"`(1 2 ~@(list 3 4))", L"(1 2 3 4)");

    assertEval(L"(defn x () (+ 1 nil))", L"nil");
    assertEval(L"(let () (defn x (y) (+ y 10)) (x 11))", L"21");

    // this was a bug where ifs could not be nested
    assertEval(L"(if true (if true 'x 'y) 'z)", L"x");

    assertEval(L"(and)", L"true");
    assertEval(L"(and nil true)", L"false");
    assertEval(L"(and 1 true)", L"true");
    assertEval(L"(and 'x 'y 'z)", L"true");
    assertEval(L"(and 'x false 'z)", L"false");

    assertEval(L"(or)", L"nil");
    assertEval(L"(or false false)", L"nil");
    assertEval(L"(or false 10)", L"10");
    assertEval(L"(or 11 false)", L"11");
    assertEval(L"(or 11 12)", L"11");
    assertEval(L"(or false 11 12)", L"11");

    assertEval(L"(not false)", L"true");
    assertEval(L"(not true)", L"false");
    assertEval(L"(not nil)", L"true");
    assertEval(L"(not 100)", L"false");

    assertEval(L"(take 3 nil)", L"nil");
    assertEval(L"(take 3 '(1 2 3 4))", L"(1 2 3)");
    assertEval(L"(take 5 '(1 2 3 4))", L"(1 2 3 4)");

    assertEval(L"(-> 10 inc (- 5) (+ 100))", L"106");

    assertEval(L"(join '(\"one\" \"two\" \"three\"))", L"\"onetwothree\"");

//    assertEval(L"(pr-str '(1 2 3))", L"\"(1 2 3)\"");
//    assertEval(L"(pr-str \"hi\")", L"\"\"hi\"\"");

//    assertEval(L"(print-str '(1 2 3))", L"\"(1 2 3)\"");
//    assertEval(L"(print-str \"hi\")", L"\"hi\"");

//    assertEval(L"(str '(1 2 3) '(4 5 6))", L"\"(1 2 3)(4 5 6)\"");
//    assertEval(L"(str \"test-\" 100)", L"\"test-100\"");

    assertEval(L"(= (get-type 'x) (get-type (symbol \"x\")))", L"true");

    assertEval(L"(do 1 2)", L"2");

    assertEval(L"(cond)", L"nil");
    assertEval(L"(cond 'x 'y)", L"y");
    assertEval(L"(cond 'false 'y)", L"nil");
    assertEval(L"(cond 'false 'y 'x 'z)", L"z");

    // TODO: this test is brittle, write better test
    // assertEval(L"(list (gensym) (gensym))", L"(gensym-1 gensym-0)")

//    assertEval(L"(= \"asdf\" (str \"as\" \"df\"))", L"true");
//    assertEval(L"(= 'asdf (symbol (str \"as\" \"df\")))", L"true");
//    assertEval(L"(= :asdf (keyword (str \"as\" \"df\")))", L"true");

    assertEval(L"(= nil nil)", L"true");
    assertEval(L"(= '(1) '(1))", L"true");
    assertEval(L"(= '(1 2) '(1 2 3))", L"false");
    assertEval(L"(= '(1 2) '(1 4))", L"false");
    assertEval(L"(= '(1 (2 3) 4) '(1 (4 5) 7))", L"false");
    assertEval(L"(= '(1 (2 3) 4) (list 1 (list 2 3) 4))", L"true");

    assertEval(L"(hash-map)", L"{}");
    assertEval(L"{}", L"{}");
    assertEval(L"(set {} :x 'y)", L"{:x y}");
    assertEval(L"(do "
               "   (def n {})"
               "   (set n :x 'y)"
               "   (get n :x))", L"y");
    assertEval(L"(:test {:test \"woah\"})", L"\"woah\"");

    assertEval(L"(vector)", L"[]");
    assertEval(L"[]", L"[]");
    assertEval(L"(set [0 0 0] 0 1)", L"[1 0 0]");
    assertEval(L"(do "
               "   (def n [0 0])"
               "   (set n 0 'y)"
               "   (get n 0))", L"y");

//    assertEval(L"(record 'cheese 1)", L"\"#cheese[]\"");
    assertEval(L"(do "
               "   (def n (record 'cheese 1))"
               "   (set n 0 'y)"
               "   (get n 0))", L"y");

    assertEval(L"(with-handler (fn err (x) :hi)"
               "   (+ 'x 'y)))", L":hi");

    assertEval(L"(with-handler (fn err (x) :hi)"
                "  (with-handler (fn err (x) :there)"
                "    (+ 'x 'y)))", L":there");

    assertEval(L"(with-handler (fn err (x) :there)"
               "  (with-handler (fn err (x) (+ 'a 'b))"
               "    (+ 'x 'y)))", L":there");

    assertEval(L"(with-handler (fn err (x) (:message x))"
               "   (throw \"WHAT\"))", L"\"WHAT\"");

    assertEval(L"(try "
               "   (throw \"yarp\")"
               "   (catch ex (:message ex)))", L"\"yarp\"");

    assertEval(L"'A'", L"'A'");

    assertEval(L"(get \"asdf\" 2)", L"'d'");

    assertEval(L"(> 10 20)", L"false");
    assertEval(L"(> 20 10)", L"true");
    assertEval(L"(>= 10 20)", L"false");
    assertEval(L"(>= 20 10)", L"true");
    assertEval(L"(>= 20 20)", L"true");
    assertEval(L"(<= 10 20)", L"true");
    assertEval(L"(<= 20 10)", L"false");
    assertEval(L"(<= 20 20)", L"true");

    assertEval(L"(map inc '(1 2 3))", L"(2 3 4)");

    assertEval(L"(split '(a b c d))", L"((a c) (b d))");
    assertEval(L"(partition '(a b c d))", L"((a b) (c d))");

    assertEval(L"(let loop (y 0)"
               "   (if (= y 5)"
               "     :done"
               "     (loop (inc y))))",
               L":done");

    // this should throw an exception, but not kill the process entirely
    assertEvalException(L"(let loop () (throw \"oops\"))");
  }
END_TEST

#define assertEvalNoStd(inputText, expectedOutputText) { \
  Error error; \
  Pool_t pool = poolCreate(ONE_MB); \
  wchar_t *result; \
  errorInitContents(&error); \
  result = NULL; \
  ck_assert_int_eq(tryReplEvalConf(pool, inputText, &result, false, config, &error), R_SUCCESS); \
  if (result != NULL) { \
    if (wcscmp(expectedOutputText, result) != 0) { \
      ck_abort_msg("got '%ls', expected '%ls'", result, expectedOutputText); \
    } \
  } \
  else { \
    ck_abort_msg("exception thrown"); \
  } \
  poolFree(pool); \
}

START_TEST(gc) {

    // this was failing because of stupid memory errors
    assertEvalNoStd(L"(let* () "
                    "   (def x (fn () \"hi\"))"
                    "   (gc)"
                    "   (gc))", L"nil");

  }
END_TEST

Suite * suite(void) {

  TCase *tc_core = tcase_create("Core");
  tcase_add_test(tc_core, basic);
  tcase_add_test(tc_core, eof_mid_number_token);
  tcase_add_test(tc_core, errors);
  tcase_add_test(tc_core, parser);
  tcase_add_test(tc_core, exprPrnTest);
  tcase_add_test(tc_core, analyzer);
  tcase_add_test(tc_core, compilerBasic);
  tcase_add_test(tc_core, vmBasic);
  tcase_add_test(tc_core, repl);
  tcase_add_test(tc_core, gc);

  if (config.gcOnAlloc) {
    tcase_set_timeout(tc_core, 60);
  }

  Suite *s = suite_create("lexer");
  suite_add_tcase(s, tc_core);

  return s;
}

int main(int argc, char** argv)
{
  vmConfigInitContents(&config);
  if (argc > 1 && strcmp("--gc-on-alloc", argv[1]) == 0) {
    config.gcOnAlloc = true;
  }

  int number_failed;
  Suite *s;
  SRunner *sr;

  s = suite();
  sr = srunner_create(s);

  srunner_run_all(sr, CK_VERBOSE);
  number_failed = srunner_ntests_failed(sr);
  srunner_free(sr);
  return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
