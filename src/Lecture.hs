module Lecture where

-- As my first lesson, I read about Type classes that are a definition that can
-- group several Types Data, so we can use special functions with this Types.
--
-- As an example: we define the following...
--
data Human = FullName String String 
           | Nickname String

simpleSalute (FullName firstName _) = "Hi, it's me, " ++ firstName ++ "!"
simpleSalute (Nickname nickname)    = "Hi, it's me, " ++ nickname ++ "!"

billGates = FullName "William" "Gates"
me        = Nickname "Lay"

tryOne = do
  putStrLn (simpleSalute billGates)
  putStrLn (simpleSalute me)

-- Now, We want to add a new data type called "Pokemos" as the same way, we want
-- to define a function to get a greeting from Human and Pokemos
--
-- This way get error:
--
data Pokemon = PokemonKind String

simpleSalute1 (FullName firstName _) = "Hi, it's me, " ++ firstName ++ "!"
simpleSalute1 (Nickname nickname)    = "Hi, it's me, " ++ nickname ++ "!"
-- simpleSalute1 (PokemonKind pokemonKind) = pokemonKind  -->> Error!!

-- billGates = FullName "William" "Gates"
-- me        = Nickname "Lay"
pikachu   = PokemonKind "Pikachu"

tryTwo = do
   putStrLn (simpleSalute1 billGates)
   putStrLn (simpleSalute1 me)
   -- putStrLn (simpleSalute1 pikachu) ---->>> this produces an error
   --
--
---------------------------------------------------------------------------
  -- Alternative Method: I won't develop |-|-|-|-|--|-|--|-|-|-|
---------------------------------------------------------------------------

-- data Human = FullName String String | Nickname String
-- data Pokemon = PokemonKind String
-- data ThingThatSalutes = Human Human | Pokemon Pokemon

-- simpleSalute :: ThingThatSalutes -> String
-- simpleSalute (Human (FullName firstName _)) = "Hi, it's me, " ++ firstName ++ "!"
-- simpleSalute (Human (Nickname nickname))    = "Hi, it's me, " ++ nickname ++ "!"
-- simpleSalute (Pokemon (PokemonKind pokemonKind)) = pokemonKind ++ "!"

-- billGates = FullName "William" "Gates"
-- me        = Nickname "Lay"
-- pikachu   = PokemonKind "Pikachu"

-- main = do
--    putStrLn (simpleSalute (Human billGates))
--    putStrLn (simpleSalute (Human me))
--    putStrLn (simpleSalute (Pokemon pikachu))


-- data Human = FullName String String 
--            | Nickname String
         
-- data Pokemon = PokemonKind String
--
class Salutable a where
  simpleSalute2 :: a -> String

-- Human's implementation of the Salutable "type class"
instance Salutable Human where
  simpleSalute2 (FullName firstName _) = "Hi, it's me, " ++ firstName ++ "!"
  simpleSalute2 (Nickname nickname) = "Hi, it's me, " ++ nickname ++ "!"

instance Salutable Pokemon where
  simpleSalute2 (PokemonKind p) = p ++ "!"

introduction x y = 
  do
    putStrLn (simpleSalute2 x)
    putStrLn (simpleSalute2 y)

tryThree = do
   introduction billGates me
   introduction billGates pikachu
