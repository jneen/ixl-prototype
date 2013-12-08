import Test.Hspec
import Test.HUnit

import Ixl.Syntax

import Text.ParserCombinators.Parsec (ParseError)
instance Eq ParseError where
  x == y = (show x == show y)

main = hspec spec

spec :: Spec
spec = do
  describe "definition" $ do
    it "defines a constant" $ do
      parseIxl "(test)" "+ x = 1" @?=
        Right (Program [Let ("x", Number 1)])

    it "defines a lambda" $ do
      parseIxl "(test)" "+ x = [ %y => $y ]" @?=
        Right (Program [Let ("x", Lambda [(VariablePattern "y", Variable "y")])])

    it "defines a lambda with multiple patterns" $ do
      parseIxl "(test)" "+ x = [ %y => $y; %z => $z ]" @?=
        Right (Program [Let ("x",
          Lambda [(VariablePattern "y", Variable "y"),
                  (VariablePattern "z", Variable "z")])])

    it "applies expressions" $ do
      parseIxl "(test)" "+ x = y $z $w" @?=
        Right (Program [Let ("x",
          Apply (Apply (Word "y") (Variable "z")) (Variable "w"))])
