# Functional Java Compiler

&copy; Kai Chen

## Overview

**FJavaC** (Functional Java Compiler) is a compiler (for x86) written in OCaml for an extended version of Java with functional features such as *higher-order functions* and *nested function compositions*. The major components include a *front-end* (a lexer and parser), the *intermediate representation* (IR), the *functional intermediate representation* (FIR), and a *back-end* (code-generator, register allocator ...). The full compiler is too huge to be covered here. In this repo, we only present parts we found of most interest.

------

## Components of Interest
* Front-end: [Lexer](./fj_ast_lex.mll), [Parser](./fj_ast_parse.mly).
* Intermediate Representation (IR): [IR](./fj_ir.ml), [Abstract Syntax Tree (ast)](./fj_ir_ast.ml), [Expressions](./fj_ir_exp.ml).
* Functional Intermediate Representation (FIR): [Continuation Passing Style (CPS) Conversion](./fj_fir_ir.ml), [Closure Conversion](./fj_fir_closure.ml).
* Back-end: [Assembly Code Generation](./x86_codegen.ml), [Liveness Analysis](./ra_live.ml), [Register Allocation](./ra_main.ml).
* Optimization: [Abstract Syntax Tree (ast) Optimization](./fj_ast_opt.ml), [Dead Code Elimination](./fj_fir_dead.ml), [Inlining/Constant Folding](./fj_fir_inline.ml).

------

## Examples
* A demonstration of the usage of higher-order functions: [HigherOrder1.java](./HigherOrder1.java).
* The assembly dump for HigherOrder1.java generated by our fjavac: [HigherOrder1.s](./HigherOrder1.s).