module FoldlST where

foldlST :: (a -> b -> a) -> a -> [b] -> a
foldlST f acc xs = runST $ do
    acc' <- newSTRef acc            -- Create a variable for the accumulator

    forM_ xs $ \x -> do             -- For each x in xs...

        a <- readSTRef acc'         -- read the accumulator
        writeSTRef acc' (f a x)     -- apply f to the accumulator and x

    readSTRef acc'                  -- and finally read the result
