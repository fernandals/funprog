module Person where

import Prelude hiding ( Either(..) )

type Name = String
type Age  = Int

data Person = Person Name Age
  deriving (Show)

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | length name < 2 = Nothing
  | age > 200       = Nothing
  | otherwise       = Just ( Person name age )

data Either a b = Left a
                | Right b
                deriving (Show, Eq)

data PersonError = NameError
                 | AgeError
                 deriving (Show, Eq)

mkPerson' :: Name -> Age -> Either PersonError Person
mkPerson' name age
  | length name < 2 = Left NameError 
  | age > 200       = Left AgeError 
  | otherwise       = Right ( Person name age )

--case fulano of 
--  Left NameError ->
--  Left AgeError ->
--  Right (Person n a) ->
--
