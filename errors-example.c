#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <unistd.h>
#include <execinfo.h>
#include <check.h>
#include "errors.h"

const char* SYSTEM = "lexer";
const char* E_INVALID_TOKEN = "invalid-token";
const char* E_EOF = "unexpected-eof";

bool dependentDoit() {

  bool failed = true;

  if (failed) {
    reportError(SYSTEM, E_EOF, L"where did this come from?: 'AARG|EOF'");
    return true;
  }

  return false;
}

bool doit() {

  if (dependentDoit()) {
    reportError(SYSTEM, E_INVALID_TOKEN, L"cannot parse a token from the remaining characters in the stream: 'AARG'");
    return true;
  }

  return false;
}

int main(void)
{

  if (doit()) {
    printf("doit failed, investigating...\n");

    printErrors();

    clearErrors();

    printErrors();
  }

  return 0;
}
