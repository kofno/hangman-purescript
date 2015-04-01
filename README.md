# hangman-purescript
Web based hangman game, written in Purescript. You know, for fun.

## Client
The cient code is written in purescript using Thermite (Reactjs). I really like the way react style code fits purescript. It reminds me a bit of working with Elm.

## Server
The server is also written in purescript using expressjs bindings and running on node. There is one big hack there to get the static content middleware to work. I had to use FFI because I couldn't figure out how to wrap it in purescript (some other day). The puzzle request handler is all purescript though.
