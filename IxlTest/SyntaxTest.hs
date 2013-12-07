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
      parseIxl "(test)" "+ x = 1" @=?
        Right (Program [Let ("x", Apply [Number 1])])

    it "defines a lambda" $ do
      parseIxl "(test)" "+ x = [ %y => $y ]" @=?
        Right (Program [Let ("x",
          Apply [Lambda [(VariablePattern "y", Apply [Variable "y"])]])])

    it "defines a lambda with multiple patterns" $ do
      parseIxl "(test)" "+ x = [ %y => $y; %z => $z ]" @=?
        Right (Program [Let ("x",
          Apply [Lambda [(VariablePattern "y", Apply [Variable "y"]),
                         (VariablePattern "z", Apply [Variable "z"])]])])

