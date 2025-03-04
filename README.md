# Janguage

> Early work in progress!

Janguage is a programming language invented by me.

Don't ask me what this is gonna be good for except for the learning process I am making through while writing this!

More information about the language will follow soon.

Here are some buzzwords that make up this language's characteristics:

* Go-like syntax (but different)
* Statically typed (at the moment there is no type checking and every variable is assumed to be of type i64)
* Easy to parse syntax (for humans and compiler as well), I guess [LL(1)](https://en.wikipedia.org/wiki/LL_parser)
* There will be multiple backends for the execution: right now there is only a stack machine, but there will be a backend using LLVM that generates all kinds of code, including a JIT compiler using LLVM ORC

> NOTE: Since I am currently looking for a new job:  
> if you are a potential employer, please note that this is a hobby project of mine and I am deliberately breaking many rules of modern/'clean' C++ programming for the sake of 'just getting it done' for now.
> There will be cleanups and refactorings at some point.
