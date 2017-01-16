import Test.Hspec

import Spec_2_3_1
import Spec_2_3_3
import Spec_2_5_2
import Spec_3_4_3

main :: IO ()
main = hspec $ do
  describe "Section 2.3.1" Spec_2_3_1.spec
  describe "Section 2.3.3" Spec_2_3_3.spec
  describe "Section 2.5.2" Spec_2_5_2.spec
  describe "Section 3.4.3" Spec_3_4_3.spec
