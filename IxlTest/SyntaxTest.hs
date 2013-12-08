import Test.Hspec
import Test.HUnit

import Ixl.Syntax

import Text.ParserCombinators.Parsec (ParseError)
instance Eq ParseError where
  x == y = (show x == show y)

main = hspec spec

spec :: Spec
spec = do
  describe "parseIxl" $ do
    it "parses a constant" $ do
      parseIxl "(test)" "1" @?=
        Right (Number 1)

    it "parses a lambda" $ do
      parseIxl "(test)" "[ %y => $y ]" @?=
        Right (Lambda [(VariablePattern "y", Variable "y")])

    it "parses a lambda with multiple patterns" $ do
      parseIxl "(test)" "[ %y => $y; %z => $z ]" @?=
        Right (Lambda [(VariablePattern "y", Variable "y"),
                       (VariablePattern "z", Variable "z")])

    it "parses applied expressions" $ do
      parseIxl "(test)" "y $z $w" @?=
        Right (Apply (Apply (Word "y") (Variable "z")) (Variable "w"))

    it "applies expressions according to parens" $ do
      parseIxl "(test)" "y ($z $w)" @?=
        Right (Apply (Word "y") (Apply (Variable "z") (Variable "w")))

    it "applies expressions with application chaining" $ do
      parseIxl "(test)" "$z $w > y" @?=
        Right (Apply (Word "y") (Apply (Variable "z") (Variable "w")))
