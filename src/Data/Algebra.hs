module Data.Algebra
  ( Algebra (..)
  ) where

-- | An algebra in the sense of set algebras. Not an algebra in the vector space
-- sense, and not related to numerical algebra - though one of the predecessor
-- structures of this 'Algebra' is called a "ring", but is not a numerical ring,
-- but instead a ring of sets.
class Algebra a where
  {-# MINIMAL unit, containedIn, (union|intersection), (difference|symmetricDifference|bar) #-}

  -- | The unit is the element of the algebra which contains all other elements
  -- prop> a `containedIn` unit
  --
  -- Additionally, it absorbs by union
  -- prop> unit `union` a == unit
  -- and is the identity for intersection
  -- prop> union `intersection` a == a
  unit :: a
  unit = bar empty

  -- | The empty is the element of the algebra which is contained in all other
  -- elements
  -- prop> empty `containedIn` a
  --
  -- It is the identity for union
  -- prop> empty `union` a == a
  -- and absorbs by intersection
  -- prop> empty `intersection` a == empty
  --
  -- By definition, it is the exclusion of the unit
  -- prop> bar unit == empty
  empty :: a
  empty = bar unit

  -- | The sub-element predicate.
  --
  -- This is reflexive
  -- prop> a `containedIn` a
  -- Transitive
  -- prop> a `containedIn` b && b `containedIn` c => a `containedIn` c
  -- But not (in general) symmetric
  containedIn :: a -> a -> Bool

  -- | The element of the algebra containing both arguments
  -- prop> (a `containedIn` b || a `containedIn` c) => a `containedIn` (b `union` c)
  union :: a -> a -> a
  union left right
    = bar (intersection (bar left) (bar right))

  -- | The element of the algebra contained in both arguments
  -- prop> a `containedIn` (b `intersection` c) <==> (a `containedIn` b && a `containedIn` c)
  intersection :: a -> a -> a
  intersection left right
    = bar (union (bar left) (bar right))

  -- | The complementary element of the algebra - equivalent to the difference
  -- between 'unit' and the element. In particular,
  -- prop> a `union` bar a == unit
  -- prop> a `intersection` bar a == empty
  bar :: a -> a
  bar = symmetricDifference unit

  -- | The element of the algebra which is contained in the left element but is
  -- nowhere in the right element
  -- prop> (a `difference` b) `containedIn` a
  -- prop> (a `difference` b) `intersection` b == empty
  difference :: a -> a -> a
  difference left right
    = intersection left (bar right)

  -- | The symmetric difference - the element of the algebra which is contained
  -- in the union, but nowhere in the intersection, of the two elements
  -- prop> (a `symmetricDifference` b) `containedIn` (a `union` b)
  -- prop> (a `symmetricDifference` b) `intersection` (a `intersection` b) == empty
  symmetricDifference :: a -> a -> a
  symmetricDifference left right
    = union (difference left right) (difference right left)
