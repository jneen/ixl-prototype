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

    it "pipes expressions" $ do
      parseIxl "(test)" "x y | z w" @?=
        Right (Pipe (Apply (Word "z") (Word "w")) (Apply (Word "x") (Word "y")))

    it "parses a let expression" $ do
      parseIxl "(test)" "+ x = $y; z" @?=
        Right (Define [Let "x" (Variable "y")] (Word "z"))

    it "parses multiple let expression" $ do
      parseIxl "(test)" "+ x = $a; + y = $b; z" @?=
        Right (Define [(Let "x" (Variable "a")),
                       (Let "y" (Variable "b"))] (Word "z"))
