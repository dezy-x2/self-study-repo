import Prelude hiding (even,odd)

-- * data type for the natural nums
data NaturalNumber = Zero | S NaturalNumber
    deriving(Show)
-- hard coded values for different numbers
zero = Zero
one = S zero
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven
nine = S eight
ten = S nine
-- sets up the use of equality operators with the natural nums
instance Eq NaturalNumber where
    Zero == Zero = True
    Zero == S _ = False
    S _ == Zero = False
    S x == S y = x == y
-- gives an order to the number
instance Ord NaturalNumber where
    compare Zero Zero = EQ 
    compare (S _) Zero = GT
    compare Zero (S _) = LT
    compare (S x) (S y) = compare x y

instance Num NaturalNumber where
    -- these first two are basically edge cases and once one thing is zero the other would be the number
    S x + Zero = S x
    Zero + S x = S x
    -- deducts from `y` and calls `S` on `x` until `y` equals `Zero` 
    S x + S y = S (S x) + y 

    -- * Multiplication

    -- edge case for if times by `Zero`
    Zero * S _ = Zero
    S _ * Zero = Zero
    -- edge case for if times by `S Zero` or one (for some reason the var `one` wasn't working idk why)
    S x * S Zero = S x
    S Zero * S x = S x
    -- this adds `S x` and `S x` together while deducting from `y` and once `y` hits `S Zero` its triggers an edge case
    S x * S y = (S x + S x) * y

    fromInteger n
        | n > 0 = S (fromInteger(n-1))
        | n == 0 = Zero
    
    -- * Subraction

    Zero - x = Zero
    x - Zero = x
    S x - S y = x - y

    -- * abs 
    abs = id

    -- * signum
    signum n
        | n > 0 = one
        | n == 0 = zero

nat :: NaturalNumber -> NaturalNumber
nat = id

odd n
    | n == zero = False
    | n == one = True
    | otherwise = odd (n-2)

even n = not (odd n)
 -- TODO: find out what remainder is and implement it
