import qualified Data.Array as A
import qualified Data.Set as S
import Data.Bits

array1 = A.array (1,10) $ zipWith (,) (S.toList . S.powerSet . S.fromList $ [(1 :: Int)..10]) (repeat (1/0))
