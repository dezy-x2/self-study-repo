data Color = Blue | Yellow | Red | Mix Color Color
    deriving(Show)

blue = Mix Blue Blue
yellow = Mix Yellow Yellow
red = Mix Red Red
orange = Mix Red Yellow
green = Mix Blue Yellow
violet = Mix Blue Red

instance Ord Color where
    compare Blue (Mix x y) = GT
    compare (Mix x y) Blue = LT
    compare Red (Mix x y) = GT
    compare (Mix x y) Red = LT
    compare Yellow (Mix x y) = GT
    compare (Mix x y) Yellow = LT
    compare (Mix x1 x2) (Mix y1 y2) = compare x1 y1
    compare Red Red = EQ 
    compare Yellow Yellow = EQ
    compare Blue Blue = EQ 
    compare Red Yellow = EQ
    compare Yellow Red = EQ
    compare Blue Yellow = EQ
    compare Yellow Blue = EQ
    compare Red Blue = EQ
    compare Blue Red = EQ

instance Eq Color where
    Red == Red = True
    Yellow == Yellow = True
    Blue == Blue = True
    Mix x1 x2 == Mix y1 y2 = ((x1 == y1) && (x2 == y2)) || ((x1 == y2) && (x2 == y1))
    x == y = False

instance Num Color where
    Mix x1 x2 + Mix y1 y2 = Mix (Mix x1 x2) (Mix y1 y2)
    x + y = error "whatever you did don't do it again"

    --TODO: figure out what subtraction should do
    --TODO: like how do you subract colors when they are set up like this?


    -- x * y = Rainbow

    -- fromInteger 1 = Red
    -- fromInteger 2 = Yellow
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
--     | color == Yellow = True
--     | color == Blue = True
--     | otherwise = False

-- baseColors :: Color -> Maybe (Color, Color)
-- baseColors color
--     | color == Green = Just (Yellow, Blue)
--     | color == Orange = Just (Yellow, Red)
--     | color == Violet = Just (Blue, Red)
--     | otherwise = Nothing

-- matchOrNo :: Color -> Color -> Bool
-- matchOrNo Green Red = True
-- matchOrNo Red Green = True
-- matchOrNo Orange Blue = True
-- matchOrNo Blue Orange = True
-- matchOrNo Yellow Violet = True
-- matchOrNo Violet Yellow = True
-- matchOrNo Rainbow Rainbow = True
-- matchOrNo Rainbow x = True
-- matchOrNo x Rainbow = True
-- matchOrNo x y = False

