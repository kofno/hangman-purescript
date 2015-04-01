module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Random

import Node.Express.Types
import Node.Express.App
import Node.Express.Handler

import Data.Array (length, (!!))
import Data.Maybe

puzzles :: [String]
puzzles = [ "Spongebob Squarepants"
          , "Chuggington"
          , "Peep In the Big Wide World"
          , "Curious George"
          , "Phineas and Ferb"
          , "Suite Life"
          , "Inspector Gadget"
          , "Spy Kids"
          , "The Croods"
          , "Gnomeo and Juliet"
          ]

handler :: Handler
handler = do
  idx <- liftEff $ randomInt 1 ((length puzzles) - 1)
  case (puzzles !! idx) of
       Just s  -> sendJson { puzzle: s }
       Nothing -> nextThrow $ error $ "Couldn't find a puzzle at: " ++ show idx

app :: App
app = do
  get "/puzzle" handler

attach :: Application -> ExpressM Unit
attach = apply app

foreign import foreignMain
  """
  function foreignMain(attachFn) {
    return function() {
      var express = require('express');
      var app     = express();
      app.use('/', express.static(__dirname + '/../../client'));
      attachFn(app)();
      var server = app.listen(process.env.PORT || 3000, function() {
        var port = server.address().port

        console.log('Hangman listening on port %s', port)
        console.log('Loading up hangman... ' + __dirname + '/../../client')
      });
    }
  }
  """ :: forall e. (Application -> ExpressM Unit) -> Eff e Unit

main = foreignMain attach
