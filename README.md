# R2llvm

R2llvm is the next generation of [RLLVMCompile](https://github.com/duncantl/RLLVMCompile) package to provide
a compiler for R code that generates machine code using [LLVM](http://llvm.org) via the Rllvm package.
This differs from RLLVMCompile as it uses
a control flow graph computed via the [rstatic](https://github.com/nick-ulle/rstatic) package
and type information computed from that control flow graph via the
[RTypeInference](https://github.com/nick-ulle/RTypeInference) package.

## Goals

The goal of this is to provide a customizable and extensible
tool that 1) people can use to compile "R-like" code, and 2)
tool developers can experiment with to explore different approaches
to making code fast.
People can use this for implementing different compilation strategies
for aspects of the R language, for targetting different platforms,
e.g. GPUs, and also for compiling different DSLs within R,
e.g., hierarchical models.

The idea is that we can translate R-like code to LLVM instructions, create those
instructions using the Rllvm package and then generate native/compiled
code and invoke it from the R session, serialize it for use in other
sessions or even applications (e.g. JavaScript, Python).

We might compile the same code in different ways for different contexts.
As a result, a single one-size-fits-all approach to compiling R is 
probably too restrictive. Furthermore, we want to be able to explore
new approaches easily w/o having to recompile all of R or learn a new
compilation framework tha is specific to R.  

This approach is very different from building a compiler
that requires changing the code of the R interpreter, or developing a
different implementation of R, i.e. separate from GNU R. 
This sits on top of the existing GNU R implementation as a package
and does not require any changes to GNU R.

One of the strengths of LLVM is that it embeddable and extensible and
provides a user-level API. A compiler for R should also do the same.
We have learned that a centralized code source that requires a core
group to make all "official" and "distributed" changes limits
innovation (but does improve stability).

LLVM also generates code for different backends, e.g., different hardware,
GPUs, etc. and emscripten can generate JavaScript from the IR code we generate
with LLVM.


## History

Vince Buffalo and I started the RLLVMCompile package several years ago (late 2010)
after the development of the Rllvm package.  Unfortunately, I had
other committments (the book XML and Web Technologies with R, and
another "Data Science in R: A Case Studies Approach to Computational
Reasoning and Problem Solving").  Nick Ulle has been working on
type inference starting in 2015/2016


