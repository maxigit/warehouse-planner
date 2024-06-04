module Here
( here
) where

import Prelude
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Char

here = QuasiQuoter { quoteExp = stringE . unlines . clean . map trim . lines } where
   trim  = dropWhile isSpace
   clean ([]:xs) = xs
   clean xs = xs
