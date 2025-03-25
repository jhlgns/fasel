cmake_minimum_required(VERSION 3.20)
project(janguage LANGUAGES CXX)

set(CMAKE_C_COMPILER /usr/local/bin/clang CACHE FILEPATH "" FORCE)
set(CMAKE_CXX_COMPILER /usr/local/bin/clang++ CACHE FILEPATH "" FORCE)
set(CMAKE_LINKER_TYPE LLD CACHE STRING "" FORCE)
add_compile_options(-std=c++20 -stdlib=libc++)
add_link_options(-std=c++20 -stdlib=libc++ -lc++abi)

find_package(LLVM REQUIRED CONFIG)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${LLVM_ROOT}/share/llvm/cmake")
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
llvm_map_components_to_libnames(llvm_libs support core irreader irprinter bitstreamreader demangle orcjit X86)
message(STATUS "llvm_libs: ${llvm_libs}")
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug)
endif()

set(shared_source_files
    compile_ir.cpp
    jit.cpp
    lex.cpp
    memory_pool.cpp
    parse.cpp
    stringify.cpp
    typecheck.cpp
    )

#
# Compiler
#

add_executable(jang main.cpp ${shared_source_files})

target_compile_definitions(jang PUBLIC ${LLVM_DEFINITIONS_LIST})
target_include_directories(jang PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${LLVM_INCLUDE_DIRS})
target_link_libraries(jang PUBLIC ${llvm_libs})
# target_compile_features(jang PUBLIC cxx_std_20)
target_compile_options(jang PUBLIC -Werror=switch)
# target_compile_options(jang PUBLIC -fsanitize=address)
# target_link_options(jang PUBLIC -fsanitize=address)

#
# Playground
#

add_executable(playground playground.cpp ${shared_source_files})
target_compile_definitions(playground PUBLIC ${LLVM_DEFINITIONS_LIST})
target_include_directories(playground PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${LLVM_INCLUDE_DIRS}
    )
target_link_libraries(playground PUBLIC ${llvm_libs})
# target_compile_features(playground PUBLIC cxx_std_20)
target_compile_options(playground PUBLIC -Werror=switch)


#
# Tests
#

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/Catch2)

add_executable(
    tests
    integration_tests.cpp
    lex_test.cpp
    parse_test.cpp
    string_util_test.cpp
    typecheck_test.cpp
    ${shared_source_files}
    )
target_compile_definitions(tests PUBLIC ${LLVM_DEFINITIONS_LIST})
target_include_directories(tests PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${LLVM_INCLUDE_DIRS}
    )
target_link_libraries(tests PUBLIC
    Catch2::Catch2WithMain
    ${llvm_libs}
    )
