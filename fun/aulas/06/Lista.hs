module Lista where

data ListaInt a b = a ++ ListaInt b
        deriving ( Show , Eq )


-- error "..."
-- error :: Show a => [Char] -> a
--
-- hd ::
-- tl ::
