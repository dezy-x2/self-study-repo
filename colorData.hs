data Color = Blue | Green | Red | Black | White | Color [Color]
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
    Black == Black = True
    White == White = True
    (Color x) == (Color y) = x == y
    x == y = False

instance Num Color where
    color1 + color2 = Color $ [color1, color2]

    --TODO: figure out what subtraction should do
    --TODO: like how do you subract colors when they are set up like this?

    (Color (x:xs)) - (Color (y:ys)) = Color $ [(subtractionHelper x y), ((Color xs) - (Color ys))]
    x - y = x

    --TODO: use fromInteger to convert primary colors to their wavelengths and then multiply those!!

    x * y = Color [White]

    fromInteger num
        | 563 < num && num < 580 = Color [Red]
        | 533 < num && num < 546 = Color [Green]
        | 419 < num && num < 440 = Color [Blue]
        | otherwise = Color [Black]

    abs = id
    signum = id

baseColors :: Color -> [Color]
baseColors (Color (x:xs)) = [x, Color (baseColors (Color xs))]
baseColors (Color []) = []


