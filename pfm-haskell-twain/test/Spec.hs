import Hello qualified
import Test.Hspec

{- FOURMOLU_DISABLE -}
{-

To run the tests, you have 2 options:

1) Use the REPL

$ cabal repl pfm-haskell-twain-test
ghci> :x :main

2) Use ghcid

$ ghcid -c "cabal repl pfm-haskell-twain-test" -T :main

 -}
{- FOURMOLU_ENABLE -}

helloSpec :: Spec
helloSpec = describe "Hello" $ do
    it "should return 'Hello, World!'" $ do
        Hello.world `shouldBe` "Hello, World!"

main :: IO ()
main = hspec $ do
    helloSpec