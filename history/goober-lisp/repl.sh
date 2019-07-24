#!/bin/bash

rlwrap -r -m '\\"' -b "(){}[],^%3@\\\";:'" go run cmd/repl/main.go
