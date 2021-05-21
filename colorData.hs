data Color = Blue | Green | Red | Mix Color Color
    deriving(Show)

blue = Mix Blue Blue
green = Mix Green Green
red = Mix Red Red
yellow = Mix Red Green
cyan = Mix Blue Green
magenta = Mix Blue Red

instance Ord Color where
    compare Blue (Mix x y) = GT
    compare (Mix x y) Blue = LT
    compare Red (Mix x y) = GT
    compare (Mix x y) Red = LT
    compare Green (Mix x y) = GT
    compare (Mix x y) Green = LT
    compare (Mix x1 x2) (Mix y1 y2) = compare x1 y1
    compare Red Red = EQ 
    compare Green Green = EQ
    compare Blue Blue = EQ 
    compare Red Green = EQ
    compare Green Red = EQ
    compare Blue Green = EQ
    compare Green Blue = EQ
    compare Red Blue = EQ
    compare Blue Red = EQ

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    Mix x1 x2 == Mix y1 y2 = ((x1 == y1) && (x2 == y2)) || ((x1 == y2) && (x2 == y1))
    x == y = False

instance Num Color where
    Mix x1 x2 + Mix y1 y2 = Mix (Mix x1 x2) (Mix y1 y2)
    x + y = error "whatever you did don't do it again"

    --TODO: figure out what subtraction should do
    --TODO: like how do you subract colors when they are set up like this?

    --TODO: use fromInteger to convert primary colors to their wavelengths and then multiply those!!

    -- x * y = Rainbow

    -- fromInteger 1 = Red
    -- fromInteger 2 = Green
    -- fromInteger 3 = Blue
    -- fromInteger 4 = Green
    -- fromInteger 5 = Orange
    -- fromInteger 6 = Violet
    -- fromInteger x = Rainbow

    -- abs = id
    -- signum = id

-- primaryOrNot :: Color -> Bool
-- primaryOrNot color 
--     | color == Red = True
--     | color == Green = True
--     | color == Blue = True
--     | otherwise = False

-- baseColors :: Color -> Maybe (Color, Color)
-- baseColors color
--     | color == Green = Just (Green, Blue)
--     | color == Orange = Just (Green, Red)
--     | color == Violet = Just (Blue, Red)
--     | otherwise = Nothing

-- matchOrNo :: Color -> Color -> Bool
-- matchOrNo Green Red = True
-- matchOrNo Red Green = True
-- matchOrNo Orange Blue = True
-- matchOrNo Blue Orange = True
-- matchOrNo Green Violet = True
-- matchOrNo Violet Green = True
-- matchOrNo Rainbow Rainbow = True
-- matchOrNo Rainbow x = True
-- matchOrNo x Rainbow = True
-- matchOrNo x y = False

