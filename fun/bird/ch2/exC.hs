import Data.Char

modernise :: String -> String 
modernise xs = unwords xs'
    where xs' = uppFst (words xs)

uppFst :: [String] -> [String]
uppFst [] = []
uppFst ((x:xs):yss) = (toUpper x : xs) : uppFst yss

