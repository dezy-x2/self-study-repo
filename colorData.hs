data Color = Blue | Green | Red | Black | Color [Color]
    deriving(Show)

blue = Color [Blue]
green = Color [Green]
red = Color [Red]
yellow = Color [Red, Green]
cyan = Color [Blue, Green]
magenta = Color [Blue, Red]

subtractionHelper :: Color -> Color -> Color
subtractionHelper color1 color2 =   if color1 == color2
                                        then Black
                                    else color1

instance Eq Color where
    Red == Red = True
    Green == Green = True
    Blue == Blue = True
    (Color x) == (Color y) = x == y
    x == y = False

instance Num Color where
    color1 + color2 = Color $ [color1, color2]

    --TODO: figure out what subtraction should do
    --TODO: like how do you subract colors when they are set up like this?

    (Color (x:xs)) - (Color (y:ys)) = Color $ [(subtractionHelper x y), ((Color xs) - (Color ys))]
    x - y = x

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

