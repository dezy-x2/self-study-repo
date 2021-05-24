-- declares the data type for colors and includes a way to make a list of colors
-- the main colors are supposed to be hidden from the user and the user would only use the colors in the list form
data Color = Blue | Green | Red | Black | White | Color [Color]
    deriving(Show)

-- preset colors 
blue = Color [Blue]
green = Color [Green]
red = Color [Red]
yellow = Color [Red, Green]
cyan = Color [Blue, Green]
magenta = Color [Blue, Red]

-- this just helps keep the num class less convoluted by putting some helper code out here
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
    -- this one is the catch all for any other situation
    x == y = False

instance Num Color where
    color1 + color2 = Color $ [color1, color2]

    (Color (x:xs)) - (Color (y:ys)) = Color $ [(subtractionHelper x y), ((Color xs) - (Color ys))]
    -- base case for if the pattern doesn't stick
    x - y = x

    -- this seemed like the best solution for what multiplying colors does 
    x * y = Color [White]

    -- tests if the number is inbetween a certain wavelength and therefore equivilent to a certain color
    fromInteger num
        | 563 < num && num < 580 = Color [Red]
        | 533 < num && num < 546 = Color [Green]
        | 419 < num && num < 440 = Color [Blue]
        | otherwise = Color [Black]

    -- idk what these would do with colors so they just return the arg
    abs = id
    signum = id

-- returns all of the existing colors in a list that is not typed in the Color type
baseColors :: Color -> [Color]
baseColors (Color (x:xs)) = [x, Color (baseColors (Color xs))]
baseColors (Color []) = []


