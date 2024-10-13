# Master Thesis

Repo that includes the most important files related to my master thesis.

- [PDF file](Thesis/LuukDeGraafThesis.pdf)
- [LaTeX source file](Thesis/source.tex)
- [references in .bib file](Thesis/refs.bib)
- [Online Library](https://studenttheses.uu.nl/handle/20.500.12932/46079)
- [Code source files](src)

## Abstract

A high-level abstraction sacrifices the ability to exercise low-level control, which can be problematic for performance critical applications. 
The phenomenon is apparent in data-parallel array languages, which traditionally do not support types with multiple variants. 

Data-parallelism uses the uniformity between multiple data elements to accelerate the process of operating on large collections of data. 
Variant types constrain the ability to operate uniformly, which therefore limits data-parallelism opportunities in the general case. 
In the situations where non-uniformity is inherit to the algorithm low-level optimizations are used to mitigate the heterogeneity. 

In this paper a higher abstraction level variant type is explored, which can capture the low-level control required to implement low-level optimizations in data-parallel languages. 
A polymorphic variant type is used to represent variance on the type-level, which can be used by data structures to adapt to the variance. 
Type-level programming is used to derive memory efficient representations for user-defined variant types. 
Custom memory representations are supported through datatype-generic programming, which automates the (de)construction of variant types. 

A fully modular variant type is presented, which can exercise low-level control while preserving the ergonomics of an existing high-level architecture. 
An implementation is provided in the data-parallel language Accelerate, which demonstrates the viability of variant types in a data-parallel context.


## Contents
```
1. Introduction
    1.1 Related Work
2. Background
    2.1 Optimizations
    2.2 Principles
    2.3 Data structures
        2.3.1 Element-wise
        2.3.2 Variant-wise
3. Polymorphic variants
    3.1 Requirements
        3.1.1 Entity-Component-System
        3.1.2 Algebraic Data Type
        3.1.3 Proposed Solution
    3.2 Type-level programming
        3.2.1 Kinds
        3.2.2 Type Family
        3.2.3 Interface
        3.2.4 Type Composition
        3.2.5 Deduplication
    3.3 Datatype-Generic programming
        3.3.1 Verifying
        3.3.2 Foundations
        3.3.3 Framework
4. Implementation
    4.1 Algebraic Data Types in Accelerate
    4.2 Result
    4.3 Benchmarks
5. Discussion
    5.1 Non-variant types
    5.2 Embedded Domain-Specific Language
    5.3 Datatype-Generic programming in Haskell
    5.4 Limitations
6. Conclusion
    6.1 Future Work
```

## Research & Contributions

The research questions answered in this
thesis are:
- How to obtain low-level control that is applicable for high performance computing, while preserving the higher-abstraction surface representation?
- What is the conceptualization of a higher-abstraction variant type, which can exercise the obtained low-level control within in a data-parallel environment?

Concretely, the contributions of this thesis include:
- An extendable deduplication algorithm for the memory representation of a variant type.
- Datatype-generic derivation of an isomorphic mapping between any two datatypes.
- A collection of variants that can be completely agnostic to its internal representation
