module Hangman.Puzzle where

import Control.Monad.Eff

import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Hangman.State

-- | Handle puzzle data coming from the backend service
data Puzzle = Puzzle String

-- | Class instance for parsing JSON responses into a puzzle
instance puzzleIsForeign :: IsForeign Puzzle where
  read value = do
    puzzle <- readProp "puzzle" value
    return $ Puzzle puzzle

foreign import puzzleGet
  """
  function puzzleGet(callback) {
    return function() {
      var ajax = new XMLHttpRequest();
      ajax.onreadystatechange = function() {
        if (ajax.readyState == 4) {
          callback(ajax.responseText)();
        }
      }
      ajax.open("GET", "/puzzle", true);
      ajax.send();
    }
  }
  """ :: forall eff a. (a -> Eff eff Unit) -> Eff eff Unit

puzzleLoader :: forall eff. (State -> Eff eff Unit) -> Eff eff Unit
puzzleLoader f = puzzleGet \txt -> f (loadedState (readJSON txt :: F Puzzle))

loadedState :: F Puzzle -> State
loadedState (Right (Puzzle s)) = State { solution : s, guesses : "", status : Playing }
loadedState _                  = State { solution : ""
                                       , guesses : ""
                                       , status : Err "Puzzle failed to load"
                                       }

