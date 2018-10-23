#!/bin/bash

gcc -c runtime.c && \
    gcc -c test.s && \
    gcc -o test test.o runtime.o
