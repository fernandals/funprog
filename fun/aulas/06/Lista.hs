module Lista where

data ListInt a b = a ++ ListInt b
        deriving ( Show , Eq )


-- error "..."
-- error :: Show a => [Char] -> a
--
-- hd ::
-- tl ::
