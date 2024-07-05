module Lib where

import System.IO.Unsafe
import Prelude hiding (break)

break = seq (unsafePerformIO getLine)


main = foldl f [] [1,2,3]
  where
    f acc a = (break a:acc)
