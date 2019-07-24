#!/bin/bash

# generate assembly
gcc -S -fno-asynchronous-unwind-tables test.c
