# Janguage

> Early work in progress!

Janguage is a programming language invented by me.

Don't ask me what this is gonna be good for except for the learning process I am making through while writing this!

More information about the language will follow soon.

Here are some characteristics that make up this language:

* Manual memory management somewhere inbetween C and C++
* Imperative (no OOP, more focus on [DOD (data oriented design)](https://en.wikipedia.org/wiki/Data-oriented_design))
* Go-like syntax (but different) that is easy to parse by humans and compilers (I guess [LL(1)](https://en.wikipedia.org/wiki/LL_parser))
* Statically typed (at the moment there is no type checking and every variable is assumed to be of type i64)
* There will be multiple backends for the execution: right now there is only a stack machine, but there will be a backend using [LLVM](https://llvm.org/) that generates all kinds of code, including a JIT compiler using [LLVM ORC](https://llvm.org/docs/ORCv2.html)

Some notes:
* It will likely soon be renamed to 'Fasel'
* No LLM was used to write this code

## The pipeline - how it will work roughly

* **Lex**: source -> tokens
* **Parse**: tokens -> AST
* **Simplify**: AST -> reduced AST
    * Loops are reduced to `if`'s with `goto`'s
    * `defer` statements are inserted before return statements
* **Typecheck**: AST -> program nodes
* **Allocate**: program nodes -> program nodes with addresses set
* (Potentially: **Optimize**: program nodes -> optimized program nodes)
* **Codegeneration**: program nodes -> VM instructions

NOTE: Maybe it is worth to convert the program nodes to [single static assignment form](https://en.wikipedia.org/wiki/Static_single-assignment_form) before code generation.

> NOTE: Since I am currently looking for a new job:  
> If you are a potential employer, please note that this is a hobby project of mine and I am deliberately breaking many rules of modern/'clean' C++ programming for the sake of 'just getting it done' for now.
> There will be cleanups and refactorings at some point.
