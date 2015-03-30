module Main where

import Control.Monad.Eff

import Node.Express.Types
import Node.Express.App
import Node.Express.Handler

handler :: Handler
handler = sendJson { puzzle: "This came from the server" }

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
      app.use('/', express.static('../client'));
      attachFn(app)();
      var server = app.listen(3000, function() {
        var port = server.address().port

        console.log('Hangman listening on port %s', port)
      });
    }
  }
  """ :: forall e. (Application -> ExpressM Unit) -> Eff e Unit

main = foreignMain attach
