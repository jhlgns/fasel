# Janguage

> Early work in progress!

Janguage is a programming language invented by me.

Don't ask me what this is gonna be good for except for the learning process I am making through while writing this!

More information about the language will follow soon.

Here are some buzzwords that make up this language's characteristics:

* Manual memory management somewhere inbetween C and C++
* Imperative (no OOP, more focus on [DOD (data oriented design)](https://en.wikipedia.org/wiki/Data-oriented_design))
* Go-like syntax (but different) that is easy to parse by humans and compilers (I guess [LL(1)](https://en.wikipedia.org/wiki/LL_parser))
* Statically typed (at the moment there is no type checking and every variable is assumed to be of type i64)
* There will be multiple backends for the execution: right now there is only a stack machine, but there will be a backend using [LLVM](https://llvm.org/) that generates all kinds of code, including a JIT compiler using [LLVM ORC](https://llvm.org/docs/ORCv2.html)
* It will probably soon be renamed to 'Fasel'

> NOTE: Since I am currently looking for a new job:  
> If you are a potential employer, please note that this is a hobby project of mine and I am deliberately breaking many rules of modern/'clean' C++ programming for the sake of 'just getting it done' for now.
> There will be cleanups and refactorings at some point.
