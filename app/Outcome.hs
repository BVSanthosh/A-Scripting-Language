module Outcome where

-- The following section of code was written by kb267, sb409 and mmc23
data Outcome = Integ Int | Flo Float | Str String | Boolean Bool | Err ErrorCase  deriving (Eq, Ord)

type ErrorCase = String
-- Ends here

-- The following section of code was written by kb267 with inputs from sb409
isInt x = x == fromInteger (round x)

-- Overriding the show method for the outcome data-type
instance Show Outcome where
  show (Integ value) = show value
  show (Flo value) = do
                        if isInt value then
                          show (round value)
                        else
                          show value
  show (Str value) = value
  show (Boolean value) = show value
  show (Err value) = value

-- Ends here
