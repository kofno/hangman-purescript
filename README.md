# hangman-purescript
Web based hangman game, written in Purescript. You know, for fun.

[Play the Game!](https://evening-refuge-5245.herokuapp.com)

NOW WITH KEYBOARD SUPPORT!

## Client
The client code is written in purescript using Thermite (Reactjs). I really
like the way react style code fits purescript. It reminds me a bit of working
with Elm.

Keyboard support has also been added recently.

## Server
The server is also written in purescript using expressjs bindings and running
on node. There is one big hack there to get the static content middleware to
work. I had to use FFI because I couldn't figure out how to wrap it in
purescript (some other day). The puzzle request handler is all purescript
though.
