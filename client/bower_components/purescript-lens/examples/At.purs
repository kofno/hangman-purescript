module Examples.Control.Lens.At where

  import Control.Lens

  import Data.Maybe

  import Debug.Trace

  import qualified Data.StrMap as SM

  foo = SM.fromList ["foo" ~ 2, "bar" ~ 3]

  main = do
    print "Starting with:"
    print foo -- fromList [Tuple ("foo") (2),Tuple ("bar") (3)]
    print "Removing `foo`"
    -- This doesn't compile, as it can't infer the `Maybe Number`.
    -- print (foo # at "foo" .~ Nothing)
    print (foo # at "foo" .~ (Nothing :: Maybe Number)) -- fromList [Tuple ("bar") (3)]
    print "Inserting `baz`"
    print (foo # at "baz" ?~ 5) -- fromList [Tuple ("foo") (2),Tuple ("bar") (3),Tuple ("baz") (5)]
    print "Inserting `baz` and incrementing"
    -- Neither of these compile.
    -- This one has an `occurs check` failure.
    -- print (foo # at "baz" ?~ 5 # ix "baz" +~ 12) -- fromList [Tuple ("foo") (2),Tuple ("bar") (3),Tuple ("baz") (17)]
    -- This one doesn't infer the type variable for `MonadState` properly.
    -- print (foo #~ do
    --   at "baz" ?= 5
    --   ix "baz" += 12) -- fromList [Tuple ("foo") (2),Tuple ("bar") (3),Tuple ("baz") (17)]
