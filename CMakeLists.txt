cmake_minimum_required(VERSION 3.12)
project(warp_lang C)

set(CMAKE_C_STANDARD 11)

#set(GCC_FLAGS "-g -fno-pie")
#SET(CMAKE_CXX_FLAGS  "${CMAKE_CXX_FLAGS} ${GCC_FLAGS}")
#SET(CMAKE_EXE_LINKER_FLAGS  "${CMAKE_EXE_LINKER_FLAGS} ${GCC_FLAGS}")

# TODO: figure out how to turn this off by default/back on when developing locally
add_compile_definitions(DEBUG=1)

find_package(PkgConfig REQUIRED)
pkg_check_modules(check REQUIRED check)

add_library(errors errors.c)
add_library(compiler reader.c analyzer.c utils.c)

add_executable(tests test.c)
target_link_libraries(tests check errors compiler)
target_compile_options(tests PRIVATE -g)

add_executable(lexer-repl lexer-repl.c)
target_link_libraries(lexer-repl errors compiler)

enable_testing()
add_test(tests tests)
