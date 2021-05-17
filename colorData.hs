data Color = Violet | Orange | Green | Blue | Yellow | Red | Rainbow
    deriving(Show, Ord)

instance Eq Color where
    Red == Red = True
    Yellow == Yellow = True
    Blue == Blue = True
    Green == Green = True
    Orange == Orange = True
    Violet == Violet = True
    x == y = False

instance Num Color where
    Red + Yellow = Orange
    Yellow + Blue = Green
    Blue + Red = Violet
    x + y = y + x

    Orange - Yellow = Red
    Orange - Red = Yellow
    Green - Yellow = Blue
    Green - Blue = Yellow
    Violet - Red = Blue
    Violet - Blue = Red

    x * y = Rainbow

    fromInteger 1 = Red
    fromInteger 2 = Yellow
    fromInteger 3 = Blue
    fromInteger 4 = Green
    fromInteger 5 = Orange
    fromInteger 6 = Violet
    fromInteger x = Rainbow

    abs = id
    signum = id

primaryOrNot :: Color -> Bool
primaryOrNot color 
    | color == Red = True
    | color == Yellow = True
    | color == Blue = True
    | otherwise = False

baseColors :: Color -> Maybe (Color, Color)
baseColors color
    | color == Green = Just (Yellow, Blue)
    | color == Orange = Just (Yellow, Red)
    | color == Violet = Just (Blue, Red)
    | otherwise = Nothing

matchOrNo :: Color -> Color -> Bool
matchOrNo Green Red = True
matchOrNo Red Green = True
matchOrNo Orange Blue = True
matchOrNo Blue Orange = True
matchOrNo Yellow Violet = True
matchOrNo Violet Yellow = True
matchOrNo Rainbow Rainbow = True
matchOrNo Rainbow x = True
matchOrNo x Rainbow = True
matchOrNo x y = False

