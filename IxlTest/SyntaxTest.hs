import Test.Hspec
import Test.HUnit

import Ixl.Syntax

import Text.ParserCombinators.Parsec (ParseError)
instance Eq ParseError where
  x == y = (show x == show y)

baseCommand = Command {
  c'target = Nothing,
  c'pipe = Nothing,
  c'terms = []
}

term t = Right (Program [baseCommand { c'terms = [t] }])

main = hspec spec

spec :: Spec
spec = do
  describe "interpolation" $ do
    it "works with a variable" $ do
      term (Interp [Variable "foo"]) @=?
        parseIxl "(input)" "\"$foo"

    it "works with a braced variable" $ do
      term (Interp [Variable "foo"]) @=?
        parseIxl "(input)" "\"${foo}"

    it "disallows dollars in braces" $ do
      case parseIxl "(input)" "\"${$foo}" of
           Left _ -> assert True
           _ -> assert False
