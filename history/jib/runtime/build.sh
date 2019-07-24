#!/bin/bash

# brew install check

cc -c -g -I runtime.h -o runtime.o runtime.c && \
cc -c -g -I runtime.h -o check_runtime.o check_runtime.c && \
cc -o check_runtime runtime.o -l check check_runtime.o && \
cc -o manual-gc-test manual-gc-test.s runtime.o

#set CK_FORK=no to more easily debug the tests
