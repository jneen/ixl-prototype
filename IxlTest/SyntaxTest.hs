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

{- --- HELPERS --- -}
term t = Program [baseCommand { c'terms = [t] }]
assertLeft x = case x of
                    Left _ -> assert True
                    _ -> assert False

main = hspec spec

spec :: Spec
spec = do
  describe "interpolation" $ do
    it "works with a variable" $ do
      Right (term (Interp [Variable "foo"])) @=?
        parseIxl "(test)" "\"$foo"

    it "works with a braced variable" $ do
      Right (term (Interp [Variable "foo"])) @=?
        parseIxl "(test)" "\"${foo}"

    it "disallows dollars in braces" $ do
      assertLeft $ parseIxl "(input)" "\"${$foo}"

  describe "subst terms" $ do
    it "works with a single word" $ do
      Right (term (Subst (term (Bareword "foo")))) @=?
          parseIxl "(test)" "(foo)"

    it "works in interpolation" $ do
      Right (term (Interp [Subst (term (Bareword "foo"))])) @=?
          parseIxl "(test)" "\"$(foo)"
