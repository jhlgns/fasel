# Fasel

> Early work in progress!

Fasel is a programming language invented by me.

Don't ask me what this is gonna be good for except for the learning process I am making through while writing this!

More information about the language will follow soon.

Here are some characteristics that make up this language:

* Manual memory management somewhere inbetween C and C++
* Imperative (no OOP, more focus on [DOD (data oriented design)](https://en.wikipedia.org/wiki/Data-oriented_design))
* Go-like syntax (but different) that is easy to parse by humans and compilers (I guess [LL(1)](https://en.wikipedia.org/wiki/LL_parser))
* Statically typed (at the moment there is no type checking and every variable is assumed to be of type i64)
* There will be multiple backends for the execution like building standalone executables, just-in-time compilation and potentially a custom stack machine.

Some notes:
* No LLM was used to write this code

## The Pipeline - How It Will Work Roughly

* **Lex**: source → tokens
* **Parse**: tokens → AST
* **Desugaring**: AST → reduced AST
    * Loops are reduced to `if`'s with `goto`'s
    * `defer` statements are inserted before return statements
* **Typecheck**: AST → program nodes
    * The sizes of all variables are now known
* (Potentially: **Optimize**: program nodes → optimized program nodes)
* **IR code generation**: program nodes → LLVM IR code

Compiled programs can then be built as standalone executables or compiled and run just-in-time.

NOTE: Maybe it is worth to convert the program nodes to [single static assignment form](https://en.wikipedia.org/wiki/Static_single-assignment_form) before code generation.

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
    cmake --build build-debug-clang && \
    cmake --install build-debug-clang --prefix /opt/llvm-debug

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
    cmake --build build-debug-clang && \
    cmake --install build-debug-clang --prefix /opt/llvm-debug
```
