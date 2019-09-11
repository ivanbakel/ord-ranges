module Data.Ranges
  ( Interval
  , single

  -- Unique patterns
  , pattern Everything

  -- Bounded
  , pattern Closed
  , pattern Open
  , pattern ClosedOpen
  , pattern OpenClosed

  -- Bounded on one end
  , pattern ToOpen
  , pattern ToClosed
  , pattern FromOpen
  , pattern FromClosed

  , containedIn
  , overlap
  , contiguous
  , isEmpty

  , intersection
  , contiguousUnion
  ) where

-- ! @ord-ranges@ - range algebra using nothing but 'Ord'
--
-- The goal of this package is to provide a range algebra API that allows for
-- range calculation over any type with just an 'Ord' implementation, unlike
-- some packages which ask for 'Enum' or other numeric types.

data Bound a
  = Inclusive a
  | Exclusive a
  | None
  deriving (Eq)

-- | Do the two bounds touch without overlapping?
touch :: (Eq a) => Bound a -> Bound a -> Bool
touch (Inclusive a) (Exclusive b) = a == b
touch (Exclusive a) (Inclusive b) = a == b
touch _             _             = False

data Lower a
  = Lower (Bound a)
  deriving (Eq)

instance (Ord a) => Ord (Lower a) where
  (Lower None) <= _ = True
  _ <= (Lower None) = False
  (Lower (Inclusive a)) <= (Lower (Inclusive b)) = a <= b
  (Lower (Inclusive a)) <= (Lower (Exclusive b)) = a <= b
  (Lower (Exclusive a)) <= (Lower (Inclusive b)) = a < b
  (Lower (Exclusive a)) <= (Lower (Exclusive b)) = a <= b

data Upper a
  = Upper (Bound a)
  deriving (Eq)

instance (Ord a) => Ord (Upper a) where
  _ <= (Upper None) = True
  (Upper None) <= _ = False
  (Upper (Inclusive a)) <= (Upper (Inclusive b)) = a <= b
  (Upper (Inclusive a)) <= (Upper (Exclusive b)) = a < b
  (Upper (Exclusive a)) <= (Upper (Inclusive b)) = a <= b
  (Upper (Exclusive a)) <= (Upper (Exclusive b)) = a <= b

-- An interval, possibly bounded or unbounded inclusively or exclusively on
-- either end. This is the basic type of the range algebra.
--
-- NB: There is no invariant that an 'Interval' is non-empty by construction,
-- or even that its lower bound is less than or equal to its upper bound.
data Interval a
  = Interval
      { lower :: Lower a
      , upper :: Upper a
      }
  deriving (Eq)

instance (Show a) => Show (Interval a) where
  show (Interval (Lower lower) (Upper upper)) = showLower <> "," <> showUpper
    where
      showLower
        | None <- lower
        = "(" <> infinity
        | Inclusive val <- lower
        = "[" <> show val
        | Exclusive val <- lower
        = "(" <> show val
      
      showUpper
        | None <- upper
        = infinity <> ")"
        | Inclusive val <- upper
        = show val <> "]"
        | Exclusive val <- upper
        = show val <> ")"

      infinity = "inf"

pattern Everything :: Interval a
pattern Everything = Interval (Lower None) (Upper None)

single :: a -> Interval a
single val = Closed val val

pattern Closed :: a -> a -> Interval a
pattern Closed lower upper
  = Interval (Lower (Inclusive lower)) (Upper (Inclusive upper))

pattern Open :: a -> a -> Interval a
pattern Open lower upper
  = Interval (Lower (Exclusive lower)) (Upper (Exclusive upper))

pattern ClosedOpen :: a -> a -> Interval a
pattern ClosedOpen lower upper
  = Interval (Lower (Inclusive lower)) (Upper (Exclusive upper))

pattern OpenClosed :: a -> a -> Interval a
pattern OpenClosed lower upper
  = Interval (Lower (Exclusive lower)) (Upper (Inclusive upper))

pattern ToOpen :: a -> Interval a
pattern ToOpen upper
  = Interval (Lower None) (Upper (Exclusive upper))

pattern ToClosed :: a -> Interval a
pattern ToClosed upper
  = Interval (Lower None) (Upper (Inclusive upper))

pattern FromOpen :: a -> Interval a
pattern FromOpen lower 
  = Interval (Lower (Exclusive lower)) (Upper None)

pattern FromClosed :: a -> Interval a
pattern FromClosed lower
  = Interval (Lower (Inclusive lower)) (Upper None)

containedIn :: (Ord a) => Interval a -> Interval a -> Bool
containedIn i1 i2 = (intersection i1 i2) == i1

overlap :: (Ord a) => Interval a -> Interval a -> Bool
overlap i1 i2 = not (isEmpty (intersection i1 i2))

contiguous :: (Ord a) => Interval a -> Interval a -> Bool
contiguous i1@(Interval (Lower l1) (Upper u1)) i2@(Interval (Lower l2) (Upper u2))
  = overlap i1 i2 || touch l1 u2 || touch l2 u1

-- | The union of two contiguous intervals - that is, two intervals whose union
-- is an interval. The union of dis-contiguous intervals is just the pair of
-- those intervals, which is not itself an interval.
--
-- If the intervals passed in are non-contiguous, the behaviour of this function
-- is not defined.
contiguousUnion :: (Ord a) => Interval a -> Interval a -> Interval a
contiguousUnion (Interval l1 u1) (Interval l2 u2)
  = Interval (min l1 l2) (max u1 u2)

-- | The intersection of two intervals - this is a total function.
intersection :: (Ord a) => Interval a -> Interval a -> Interval a
intersection (Interval l1 u1) (Interval l2 u2)
  = Interval (max l1 l2) (min u1 u2)

isEmpty :: (Ord b) => Interval b -> Bool
isEmpty (Interval (Lower None) _) = False
isEmpty (Interval _ (Upper None)) = False
isEmpty (Closed l u) = u < l
isEmpty (ClosedOpen l u) = u <= l
isEmpty (OpenClosed l u) = u <= l
isEmpty (Open l u) = u <= l
