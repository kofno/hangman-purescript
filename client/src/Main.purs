module Main where

import qualified Thermite as T
import qualified Thermite.Types as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A

import Optic.Core ((..), (++~))

import Control.Monad.Eff
import Data.String (indexOf, split, joinWith, toUpper)
import Data.Array (length, map)

import Hangman.State
import Hangman.Puzzle

data Action = Guess String
            | Load

foreign import getKeyPressed
  "function getKeyPressed(e) {\
  \  return String.fromCharCode(e.keyCode);\
  \}" :: T.KeyboardEvent -> String 

render :: T.Render State _ Action
render ctx state@(State st) _ =
  T.div [ A.className "hangman"
        , A.tabIndex "1"
        , T.onKeyDown ctx (Guess <<< toUpper <<< getKeyPressed)
        ] [header, letterButtons, maskedSolution, gallows, statusBar]
    where
      header :: T.Html _
      header = T.div [ A.className "pure-menu pure-menu-horizontal" ]
                     [ T.h1 [ A.className "pure-menu-heading" ] [ T.text "Hangman: Purescript" ]
                     , T.ul [ A.className "pure-menu-list" ]
                            [ T.li [ A.className "pure-menu-item" ]
                                   [ T.a [ A.href "https://github.com/kofno/hangman-purescript"
                                         , A.className "pure-menu-link"
                                         ] [T.text "Source"]
                                   ]
                            ]
                     ]

      letterButtons :: T.Html _
      letterButtons = T.div [A.className "btn-grp"] $ map letterButton letters

      letterButton :: String -> T.Html _
      letterButton l = T.button [ A.className (letterClass l)
                                , T.onClick ctx (\_ -> Guess l)
                                , A.disabled (isGuessed l || gameOver st.status)
                                ] [ T.text l ]

      statusBar :: T.Html _
      statusBar = T.div [A.className "status"] [statusMsg st.status, newGame]

      statusMsg :: Status -> T.Html _
      statusMsg (Err s)  = T.div [ A.className "error" ] [ T.strong' [T.text "Error! "]
                                                         , T.text s
                                                         ]
      statusMsg (Won s)  = T.div [ A.className "good"  ] [ T.strong' [T.text "Victory! "]
                                                         , T.text s
                                                         ]
      statusMsg (Lost s) = T.div [ A.className "bad"   ] [ T.strong' [T.text "*Sad Trombone* "]
                                                         , T.text s
                                                         ]
      statusMsg _        = T.div [] []

      newGame :: T.Html _
      newGame = T.button [T.onClick ctx (\_ -> Load)] [T.text "New Game"]

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
      mask c = if isGuessed (toUpper c) || gameOver st.status
                  then c
                  else "_"

      gallows :: T.Html _
      gallows = T.div [A.className "gallows"]
                      [T.img [ A.src gallowsSrc
                             , A.alt "Gallows"
                             ] []
                      ]

      gallowsSrc :: String
      gallowsSrc = case length (misses state) of
                        0 -> "hm0.png"
                        1 -> "hm1.png"
                        2 -> "hm2.png"
                        3 -> "hm3.png"
                        4 -> "hm4.png"
                        5 -> "hm5.png"
                        _ -> "hm6.png"

performAction :: T.PerformAction _ Action (T.Action _ State)
performAction _ Load   = T.asyncSetState puzzleLoader
performAction _ action = T.modifyState (updateStatus <<< updateState action)
  where
    updateState :: Action -> State -> State
    updateState (Guess l)  = _State .. guesses ++~ l

    updateStatus :: State -> State
    updateStatus state@(State st) = State st { status = gameStatus }
      where
        gameStatus :: Status
        gameStatus = if length (misses state) > 5
                        then (Lost "Awww. Would you like to try again?")
                        else if solved state
                                  then (Won "Great Game! How about another?")
                                  else Playing

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount Load

main = do
  let component = T.createClass spec
  T.render component {}

