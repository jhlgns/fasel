# Fasel

_**F**ast **A**nd **S**imple **E**ngramming **L**anguage_ - or from German verb "_faseln_" (to babble)

> **Early work in progress!**

Fasel is a programming language and compiler/JIT compiler invented by me, Jan Hahlganß.

Don't ask me what this is gonna be good for except for the learning process I am making through while writing this!

More information about the language will follow soon.

Here are some characteristics that make up this language:

* General purpose - can be used for writing kernel modues as well as games and web-apps and SPAs.
* Manual memory management somewhere inbetween C and C++
* Imperative (no OOP, more focus on [DOD (data oriented design)](https://en.wikipedia.org/wiki/Data-oriented_design))
* Go-like syntax (but different) that is easy to parse by humans and compilers (I guess [LL(1)](https://en.wikipedia.org/wiki/LL_parser))
* Statically typed with fewer implicit casts than other languages [sic]
* It will compile to all major CPU architectures, including AMD64, X86, ARM, WASM, RISC-V - [most of the ones that LLVM supports](https://github.com/llvm/llvm-project/tree/main/llvm/lib/Target).

## Progress And Documentation

You can find example programs that serve as integration tests in the [integration-tests](./integration-tests) folder.

There is also a [program](./generate_bogus_program.fsl) that will serve to generate benchmarks for the parser by generating random programs (WIP).

## Notes On Building LLVM

* Make sure your system wide default compiler is Clang++ with C++20 support such that `libc++` is linked as the C++ standard library when building LLVM
    * On Debian systems use `update-alternatives` to set `cc`/`c++` → `clang++` and `clang++` → `clang++-21`

```bash
# Build in debug (requires substantially more disk space!)
cmake \
    -S llvm \
    -B build-debug \
    -G Ninja \
    -DCMAKE_BUILD_TYPE=Debug \
    -DLLVM_ENABLE_PROJECTS="clang" \
    -DLLVM_TARGETS_TO_BUILD="ARM;WebAssembly;X86" \
    -DLLVM_PARALLEL_COMPILE_JOBS=$(nproc) \
    -DLLVM_PARALLEL_LINK_JOBS=1 \
    -DLLVM_USE_LINKER="lld" \
    -DLLVM_ENABLE_LIBCXX=On \
    -DCMAKE_COMPILE_FLAGS="-std=c++20 -stdlib=libc++" \
    -DCMAKE_LINK_FLAGS="-std=c++20 -stdlib=libc++" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && \
    cmake --build build-debug && \
    cmake --install build-debug --prefix /opt/llvm-debug

# Build in release
cmake \
    -S llvm \
    -B build-release \
    -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DLLVM_ENABLE_PROJECTS="clang" \
    -DLLVM_TARGETS_TO_BUILD="ARM;WebAssembly;X86" \
    -DLLVM_PARALLEL_COMPILE_JOBS=$(nproc) \
    -DLLVM_PARALLEL_LINK_JOBS=1 \
    -DLLVM_USE_LINKER="lld" \
    -DLLVM_ENABLE_LIBCXX=On \
    -DCMAKE_COMPILE_FLAGS="-std=c++20 -stdlib=libc++" \
    -DCMAKE_LINK_FLAGS="-std=c++20 -stdlib=libc++" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && \
    cmake --build build-release && \
    cmake --install build-release --prefix /opt/llvm-release
```
