module Jeep.Control.Monad where

-----------------------------------

-- https://www.reddit.com/r/haskellquestions/comments/3akbeq/comment/cse5zvz/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button

iterateM :: Monad m => Int -> (a -> m a) -> m a -> m [a]
iterateM n f = sequence . take n . iterate (>>= f)

-- End ---------------------------------------------------------------