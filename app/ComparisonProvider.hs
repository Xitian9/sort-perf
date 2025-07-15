module ComparisonProvider (ComparisonTest(..)) where

import Test.Tasty.Providers (IsTest(..), testPassed)
import Data.Data (Typeable)
import Data.Tagged
import Data.IORef (newIORef, modifyIORef')
import GHC.IO (unsafePerformIO, evaluate)
import Control.DeepSeq (NFData(rnf))
import GHC.IORef (readIORef)

data ComparisonTest a b = ComparisonTest
  { list :: ![a]
  , alg  :: !((a -> b) -> [a] -> [a])
  , proj :: !(a -> b)
  }

instance (Typeable a, Typeable b, Ord b, NFData a) => IsTest (ComparisonTest a b) where
  run _ (ComparisonTest l sortOn p) _ = do
    comps <- comparisons sortOn p l
    pure $ testPassed (show comps ++ " comparisons")

  testOptions = Tagged []

comparisons :: (NFData t, Ord b) => ((a -> b) -> t -> t) -> (a -> b) -> t -> IO Int
comparisons sortOnF p xs = do
    v <- newIORef 0
    let proj' a = unsafePerformIO $ do
            modifyIORef' v succ
            pure $ p a
    evaluate $ rnf $ sortOnF proj' xs
    readIORef v

