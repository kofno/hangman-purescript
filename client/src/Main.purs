module Main where

import qualified Thermite as T
import qualified Thermite.Types as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A

import Optic.Core (LensP(), (..), (++~))

import Data.Array (map, length, snoc)
import Data.String (indexOf, split, joinWith, toUpper)
import Data.Foldable (foldr, elem)

import Debug.Trace

data Action = Guess String

------------ State and some Lenses --------------------
data State = State { guesses :: String
                   , solution :: String
                   }

_State :: LensP State { guesses :: _, solution :: _ }
_State f (State st) = State <$> f st

guesses :: forall r. LensP { guesses :: _ | r } _
guesses f st = f st.guesses <#> \i -> st { guesses = i }

--------------------------------------------------------

initialState :: State
initialState = State { guesses : ""
                     , solution : "This is just a placeholder"
                     }

render :: T.Render State _ Action
render ctx (State st) _ =
  T.div [A.className "hangman"] [letterButtons, maskedSolution, gallows]
    where
      letterButtons :: T.Html _
      letterButtons = T.div [A.className "btn-grp"] $ map letterButton letters

      letterButton :: String -> T.Html _
      letterButton l = T.button [ A.className (letterClass l)
                                , T.onClick ctx (\_ -> Guess l)
                                , A.disabled (isGuessed l)
                                ] [ T.text l ]

      isGuessed :: String -> Boolean
      isGuessed l = indexOf l st.guesses /= -1

      letterClass :: String -> String
      letterClass l = if (isGuessed l)
                           then "btn guessed"
                           else "btn"

      letters :: [String]
      letters = split "" "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

      maskedSolution :: T.Html _
      maskedSolution = T.div [A.className "solution"] [T.text $ joinWith " " (map mask $ split "" st.solution)]

      mask :: String -> String
      mask " " = " * "
      mask c = if isGuessed (toUpper c)
                  then c
                  else "_"

      gallows :: T.Html _
      gallows = T.div [A.className "gallows"]
                      [T.img [ A.src gallowsSrc
                             , A.alt "Gallows"
                             ] []
                      ]

      gallowsSrc :: String
      gallowsSrc = case length misses of
                        0 -> "hm0.png"
                        1 -> "hm1.png"
                        2 -> "hm2.png"
                        3 -> "hm3.png"
                        4 -> "hm4.png"
                        5 -> "hm5.png"
                        _ -> "hm6.png"

      misses :: [String]
      misses = foldr miss [] (split "" st.guesses)
        where
          miss :: String -> [String] -> [String]
          miss s seen = if hit s || s `elem` seen
                           then seen
                           else seen `snoc` s

          hit :: String -> Boolean
          hit s = toUpper s `elem` split "" (toUpper st.solution)

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
