module Hangman.State
 ( Status(..)
 , State (..)
 , _State
 , guesses
 , initialState
 , gameOver
 , misses
 , solved
 ) where

import Optic.Core (LensP())

import Data.String (split, toUpper)
import Data.Foldable (foldr, elem)
import Data.Array (snoc, filter)

data Status = Playing
            | Won String
            | Lost String
            | Err String

------------ State and some Lenses --------------------
data State = State { guesses  :: String
                   , solution :: String
                   , status   :: Status
                   }

_State :: LensP State { guesses :: _, solution :: _, status :: _ }
_State f (State st) = State <$> f st

guesses :: forall r. LensP { guesses :: _ | r } _
guesses f st = f st.guesses <#> \i -> st { guesses = i }

--------------------------------------------------------

initialState :: State
initialState = State { guesses  : ""
                     , solution : ""
                     , status   : Playing
                     }

gameOver :: Status -> Boolean
gameOver Playing = false
gameOver _       = true

misses :: State -> [String]
misses (State st) = foldr miss [] (split "" st.guesses)
  where
    miss :: String -> [String] -> [String]
    miss s seen = if hit s || s `elem` seen
                     then seen
                     else seen `snoc` s

    hit :: String -> Boolean
    hit = anyCaseMatch st.solution

solved :: State -> Boolean
solved (State st) = split "" st.solution == filter guessed (split "" st.solution)
  where
    guessed :: String -> Boolean
    guessed = anyCaseMatch st.guesses

-- | Generalize impl of hit
anyCaseMatch :: String -> String -> Boolean
anyCaseMatch ss s = (toUpper s `elem` split "" (toUpper ss)) || s == " "
