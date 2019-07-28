module WeighBench.Encode where

import           FlatBuffers.Write
import           Types
import           Weigh

groups :: Weigh ()
groups =
  wgroup "Write.encode" $
    func "scalars" encode $
      scalars
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
          (Just 1234.56) (Just 2873242.82782) (Just True) $ Just $
        scalars
            (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
            (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
            (Just 1234.56) (Just 2873242.82782) (Just True) $ Just $
          scalars
            (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
            (Just maxBound) (Just maxBound) (Just maxBound) (Just maxBound)
            (Just 1234.56) (Just 2873242.82782) (Just True) Nothing

