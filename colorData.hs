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

    -- x * y = 

    fromInteger num
        | 563 < num && num < 580 = Color [Red]
        | 533 < num && num < 546 = Color [Green]
        | 419 < num && num < 440 = Color [Blue]
        | otherwise = Black

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


