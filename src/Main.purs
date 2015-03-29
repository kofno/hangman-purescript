module Main where

import qualified Thermite as T
import qualified Thermite.Types as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A

import Optic.Core (LensP(), (..), (++~))

import Data.Array (map)
import Data.String (indexOf, fromChar, toCharArray)

import Debug.Trace

data Action = Guess String

------------ State and some Lenses --------------------
data State = State { guesses :: String
                   }

_State :: LensP State { guesses :: _ }
_State f (State st) = State <$> f st

guesses :: forall r. LensP { guesses :: _ | r } _
guesses f st = f st.guesses <#> \i -> st { guesses = i }

--------------------------------------------------------

initialState :: State
initialState = State { guesses : "" }

render :: T.Render State _ Action
render ctx (State { guesses : g }) _ =
  T.div [A.className "hangman"] [letterButtons]
    where
      letterButtons :: T.Html _
      letterButtons = T.div [A.className "btn-grp"] $ map letterButton letters

      letterButton :: String -> T.Html _
      letterButton l = T.button [ A.className (letterClass l g)
                                , T.onClick ctx (\_ -> Guess l)
                                , A.disabled (isGuessed l g)
                                ] [ T.text l ]

      isGuessed :: String -> String -> Boolean
      isGuessed l g = indexOf l g /= -1

      letterClass :: String -> String -> String
      letterClass l g = if (isGuessed l g)
                           then "btn guessed"
                           else "btn"

      letters :: [String]
      letters = map fromChar (toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

performAction :: T.PerformAction _ Action (T.Action _ State)
performAction _ action = T.modifyState (updateState action)
  where
    updateState :: Action -> State -> State
    updateState (Guess l)  = _State .. guesses ++~ l


spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render

main = do
  let component = T.createClass spec
  T.render component {}
