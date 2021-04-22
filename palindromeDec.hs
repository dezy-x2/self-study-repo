main = interact palindrome


palindrome resp = 
    unlines $ map (\x -> if (reverse x) == x then "palindrome" else "not palindrome") respLines
    where respLines = lines resp