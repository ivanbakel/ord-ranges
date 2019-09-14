# ord-ranges - range algebra only using Ord

`ord-ranges` is a package for doing algebra on ranges (i.e combinations of intervals) based on nothing but a correctly-implemented total order.

It lets you do all the normal operations of union, intersection, inversion, and element testing, without needing additional classes like `Num`, `Enum`, or `Bounded`.

## Base types

The most basic type is the `Interval`, which is a contiguous sub-segment of the line from negative infinity (for your `Ord` datatype) to positive infinity.

## Using the algebra

The operations on the algebra are members of the `Algebra` class, implemented for any `MonadPlus, Foldable` container of `Interval`s (such as `[Interval]`). The instance is defined in `Data.Algebra.Ranges`.

## The Algebra class

`Algebra` is a class describing the features of an algebra of sets.
