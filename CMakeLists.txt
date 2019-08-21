cmake_minimum_required(VERSION 3.10.2)
project(warp_lang C)

set(CMAKE_C_STANDARD 11)

include_directories(/usr/local/include)
link_directories(/usr/local/lib)

# TODO: figure out how to turn this off by default/back on when developing locally
#set(CMAKE_VERBOSE_MAKEFILE ON)

add_definitions(-DDEBUG=1 -DSTD_LIB="${CMAKE_CURRENT_LIST_DIR}/core.lsp")

find_package(PkgConfig REQUIRED)
find_package(Threads REQUIRED)
pkg_check_modules(check REQUIRED check)

add_library(compiler errors.c bootstrap/reader.c bootstrap/analyzer.c vm/vm.c bootstrap/utils.c bootstrap/compiler.c bootstrap/lexer.c bootstrap/repl.c bootstrap/source.c bootstrap/expander.c bootstrap/pool.c bytecode.c bootstrap/print.c bootstrap/print.h bootstrap/ast.h bootstrap/ast.c)

add_executable(tests test.c)
target_link_libraries(tests check compiler)

if (UNIX AND NOT APPLE)
    target_link_libraries(tests -lm -lrt ${CMAKE_THREAD_LIBS_INIT} -lsubunit)
endif()

target_compile_options(tests PRIVATE -g)

add_executable(lexer-repl bootstrap/lexer-repl.c)
target_link_libraries(lexer-repl compiler)

add_executable(repl-main bootstrap/repl-main.c)
target_link_libraries(repl-main compiler)
target_compile_options(repl-main PRIVATE -g)

enable_testing()
add_test(tests tests)
add_test(tests-gc tests --gc-on-alloc)
