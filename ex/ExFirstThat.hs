module ExFirstThat where

maybeHead :: [a] -> Maybe a
maybeHead []    = Nothing
maybeHead (x:_) = Just x

-- Q: Who is the first of these that has the property p?
-- Possible answers
-- * Nobody
-- * Just that guy
firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat p = maybeHead . filter p 

maybeCheck :: (a -> Bool) -> Maybe a -> Maybe Bool
maybeCheck p Nothing  = Nothing
maybeCheck p (Just x) = Just (p x)

-- Q: What do you have to say about the first of these that has property p?  Do you like them or not?
-- Possible answers:
-- * Just a simple (yes/no)
-- * Nothing to say (cause nobody has this property)
isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat ok p = maybeCheck ok . firstThat p 
