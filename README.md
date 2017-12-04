TypeInference
=============

This was the project for my 4th year of my Masters in Computer Science at Oxford. It implements an interpreter for [GeomLab](https://www.cs.ox.ac.uk/geomlab/home.html) which performs dynamic type inference on expressions. It does this by extending Hindley-Milner type inference to produce types that depend on the unknown types of other definitions.

## Dynamic Type Inference?

In type inference, you are given an expression for which you produce a type (or fail for invalid expressions). Usually, this requires having the types of all variables the expression depends on, for example performing type inference on `define f(x) = g(10, x)` would require knowing the type of `g`. In this interpreter that is not necessary, instead the type of `f` becomes dependent on `g`'s unknown type. The type returned is `∀α.∀β.(α → β, {g : (Number, α) → β})`.

## State of this repo

I haven't touched this code since completing my degree, only moved it out of the discontinued FPComplete into GitHub. There are a few things I would do differently now, but I'm keeping this as-is for the record. I'm not sure if it still builds, contact me if you want to try it and can't get it to work.
