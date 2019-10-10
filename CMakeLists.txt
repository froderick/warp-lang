cmake_minimum_required(VERSION 3.10.2)
project(warp_lang C)

set(CMAKE_C_STANDARD 11)

include_directories(/usr/local/include)
link_directories(/usr/local/lib)

add_definitions(-DDEBUG=1 -DSTD_LIB="${CMAKE_CURRENT_LIST_DIR}/src/core.lsp")

find_package(PkgConfig REQUIRED)
find_package(Threads REQUIRED)
pkg_check_modules(check REQUIRED check)

add_library(compiler
  src/errors.c
  src/bytecode.c
  src/bootstrap/reader.c
  src/bootstrap/analyzer.c
  src/bootstrap/utils.c
  src/bootstrap/compiler.c
  src/bootstrap/lexer.c
  src/bootstrap/repl.c
  src/bootstrap/source.c
  src/bootstrap/expander.c
  src/bootstrap/pool.c
  src/bootstrap/print.c
  src/bootstrap/print.h
  src/bootstrap/ast.h
  src/bootstrap/ast.c
  src/vm/vm.c
  src/vm/symbol.c
  src/vm/value.c
  src/vm/builtin.c
  src/vm/eval.c
  src/vm/heap.c
  src/vm/frame.c
  src/vm/exception.c)

add_executable(tests src/test.c)
target_link_libraries(tests check compiler)

if (UNIX AND NOT APPLE)
    target_link_libraries(tests -lm -lrt ${CMAKE_THREAD_LIBS_INIT} -lsubunit)
endif()

target_compile_options(tests PRIVATE -g)

add_executable(lexer-repl src/bootstrap/lexer-repl.c)
target_link_libraries(lexer-repl compiler)

add_executable(repl-main src/bootstrap/repl-main.c)
target_link_libraries(repl-main compiler)
target_compile_options(repl-main PRIVATE -g)

enable_testing()
add_test(tests tests)
add_test(tests-gc tests --gc-on-alloc)
