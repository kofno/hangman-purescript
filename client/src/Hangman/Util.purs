module Hangman.Util
  ( anyCaseMatch
  ) where

import Data.Foldable (elem)
import Data.String   (toUpper, split)

-- | Generalize impl of hit
anyCaseMatch :: String -> String -> Boolean
anyCaseMatch ss s = (toUpper s `elem` split "" (toUpper ss)) || s == " "
