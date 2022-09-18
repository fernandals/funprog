module RPG where

-- Improvise!  A Character could have, for example:
-- a Name, a Race, a Class, a Level, XP (experience points),
-- 6 attributes:
--   Strength Intelligence Wisdom Dexterity Constitution Charisma
-- current and total HP (hit points)
-- current and total MP (mana points)
-- current and total GP (gold pieces)
data Character = Character{ name :: String, race :: String, class :: String, level :: Int, XP :: Int, HP :: (Int, Int), MP :: (Int, Int), GP :: (Int, Int), skills :: [Skill]}

-- does that make sense?
type Party = [Character]

-- gets a character and returns one that is the same but +1 level
gainLevel :: Character -> Character
gainLevel player = level player + 1 

-- to be used when a character is hit
hitCharacter :: undefined
hitCharacter = undefined

alive :: Character -> Bool
alive player = (fst HP) > 0 


-- How would you implement skills and spells?

data Skill = Strength Float
           | Intelligence Float
           | Wisdom Float
           | Dexterity Float
           | Constitution Float
           | Charisma Float

data Spell

skills :: Character -> [Skill]
skills = undefined

spells :: Character -> [Spell]
spells = undefined

