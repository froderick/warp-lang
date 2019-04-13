cmake_minimum_required(VERSION 3.10.2)
project(warp_lang C)

set(CMAKE_C_STANDARD 11)

include_directories(/usr/local/include)
link_directories(/usr/local/lib)

# TODO: figure out how to turn this off by default/back on when developing locally
#set(CMAKE_VERBOSE_MAKEFILE ON)

add_definitions(-DDEBUG=1 -DSTD_LIB="${CMAKE_CURRENT_LIST_DIR}/core.lsp")

find_package(PkgConfig REQUIRED)
pkg_check_modules(check REQUIRED check)

add_library(compiler errors.c reader.c analyzer.c vm.c utils.c compiler.h compiler.c lexer.h lexer.c source.h repl.h repl.c source.c expander.h expander.c)

add_executable(tests test.c)
target_link_libraries(tests check compiler)
target_compile_options(tests PRIVATE -g)

add_executable(lexer-repl lexer-repl.c)
target_link_libraries(lexer-repl compiler)

add_executable(repl-main repl-main.c)
target_link_libraries(repl-main compiler)
target_compile_options(repl-main PRIVATE -g)

enable_testing()
add_test(tests tests)
