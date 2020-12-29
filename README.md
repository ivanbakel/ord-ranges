# ARCHIVAL

This repository was originally intended to go on Hackage as a replacement for `range`, which at the time had an API largely based on `Enum`, and therefore assumed discrete values in its ranges. This had lots of other negative consequences - for example, range bounds were only ever closed (inclusive).  Since I needed ranges for `Scientific`, an arbitrary-precision decimal type, `range` was not fit for use.

Since I wrote this, `range` has been updated to be much better - the reliance on `Enum` has been dropped, and range ends can be open/closed. That means that `ord-ranges` is no longer necessary. If you are looking for this kind of API, I can highly recommend `range`.

# ord-ranges - range algebra only using Ord

`ord-ranges` is a package for doing algebra on ranges (i.e combinations of intervals) based on nothing but a correctly-implemented total order.

It lets you do all the normal operations of union, intersection, inversion, and element testing, without needing additional classes like `Num`, `Enum`, or `Bounded`.

## Base types

The most basic type is the `Interval`, which is a contiguous sub-segment of the line from negative infinity (for your `Ord` datatype) to positive infinity.

## Using the algebra

The operations on the algebra are members of the `Algebra` class, implemented for any `MonadPlus, Foldable` container of `Interval`s (such as `[Interval]`). The instance is defined in `Data.Algebra.Ranges`.

## The Algebra class

`Algebra` is a class describing the features of an algebra of sets.
