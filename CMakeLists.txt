cmake_minimum_required(VERSION 3.20)
project(janguage LANGUAGES CXX)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

set(CMAKE_C_COMPILER /usrbin/clang-18 CACHE FILEPATH "" FORCE)
set(CMAKE_CXX_COMPILER /usr/bin/clang++-18 CACHE FILEPATH "" FORCE)
add_compile_options(-std=c++20 -stdlib=libc++)
add_link_options(-std=c++20 -stdlib=libc++)

add_executable(jang
    main.cpp
    arena.cpp
    compile.cpp
    lex.cpp
    parse.cpp
    stringify.cpp
    test.cpp
    vm.cpp
    )
target_include_directories(jang PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
# target_compile_options(jang PUBLIC -fsanitize=address)
# target_link_options(jang PUBLIC -fsanitize=address)

target_compile_features(jang PUBLIC cxx_std_20)

