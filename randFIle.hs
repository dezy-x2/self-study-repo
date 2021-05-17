import Control.Applicative

doubleUp x = x * 2;

isFive :: a -> b
isFive 5 = "It's a Five!!"
isFive x = "WRONG"

multList' [] = 1
multList' (f:r) = f * multList' r

goodDay ans
    | ans == "Yes" = "Good I hope I can ruin it"
    | ans == "No" = "Good now I don't have to ruin it"
    | otherwise = "That's not even an option idiot"

initials (f:_) (l:_) = [f] ++ " " ++ [l]

miny' x y 
    | x < y = x
    | otherwise = y

min' :: (Ord a) => [a] -> a  
min' [] = error "Empty list has no min"
min' [x] = x
min' (r:est) = miny' r (min' est)

-- rep :: (Int a, Show b) => a -> b -> [b]
rep amount item 
    | amount <= 0 = []
    | otherwise = item:rep (amount - 1) item

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' amount list
    | amount < 0 = error "cannot take negative elems"
    | amount == 0 = []
    | otherwise = (head list):take' (amount - 1) (tail list)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

elem' :: Eq t => t -> [t] -> Bool
elem' _ [] = False
elem' elm (x:xs) 
    | elm == x = True
    | otherwise = elem' elm xs

filter' _ [] = []
filter' f all = [ex | ex <- all, f ex]

makeChain' :: (Integral a) => a -> [a]  
makeChain' num 
    | num == 1 = [1]
    | num `mod` 2 == 0 = num:makeChain' (num `div` 2)
    | otherwise = num:makeChain' (num * 3 + 1)

lotsOfChain = length (filter isLong (map makeChain' [1..100]))
    where isLong xs = length xs > 15

timesToHundred :: (Num a, Enum a) => a -> [a]
timesToHundred n = map ($ n) $ all
    where all = map (*) [1..100]

searchForZ :: [Char] -> Bool
searchForZ = elem' 'Z'

totalResults :: (Ord a, Num a) => [a] -> Int
totalResults xs = length $ filter' (\x -> x > 5) xs

intersperce' _ [] = []
intersperce' char (x:xs) = x:char: (intersperce' char xs)

--stuff
data Bool' = Truth | Lie

instance Show Bool' where
    show Truth = "That is the truth"
    show Lie = "You are lying!"

equate' :: (Eq a) => a -> a -> Bool'
equate' a b = if a == b then Truth else Lie

data Possibly a = Here a | Nada deriving (Show)

instance Functor Possibly where
    fmap f (Here a) = Here $ f a
    fmap f Nada = Nada

class ColorId a where
    colorId :: a -> Bool'

instance ColorId Bool where
    colorId True = Truth
    colorId _ = Lie

instance ColorId Int where
    -- colorId :: (Int a) => a -> Bool' 
    colorId 1 = Truth
    colorId 2 = Truth
    colorId 3 = Truth
    colorId _ = Lie 

instance ColorId [a] where
    colorId [] = Lie
    colorId _ = Truth

solveRPN :: (Num a, Read a) => String -> a
solveRPN expression = head $ foldl foldFunc [] $ words expression
    where   foldFunc (x:y:xs) "*" = (x * y):xs
            foldFunc (x:y:xs) "+" = (x + y):xs
            foldFunc (x:y:xs) "-" = (x - y):xs
            foldFunc x aNumber = (read aNumber):x 

-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing = Nothing

-- instance Applicative Maybe where
--     pure = Just
--     Nothing <*> _ = Nothing
--     (Just f) <*> something = fmap f something

to :: (Eq a, Num a) => a -> a -> [a]
to a b 
    | a == b = b:[]
    | otherwise = a:((a+1) `to` b)

change :: Char -> Char
change = id