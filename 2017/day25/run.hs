{-# LANGUAGE RecordWildCards #-}
data Machine = M { tape  :: !(Zipper Bool)
                 , steps :: !Int
                 , state :: !Node }

data Zipper a = Z [a] a [a]
  deriving Show

instance Functor Zipper where
  fmap f (Z l c r) = Z (f <$> l) (f c) (f <$> r)

instance Applicative Zipper where
  pure x = Z (repeat x) x (repeat x)
  Z lf cf rf <*> Z lx cx rx = Z (zipWith ($) lf lx) (cf cx) (zipWith ($) rf rx)

set :: a -> Zipper a -> Zipper a
set v (Z l _ r) = Z l v r

current :: Zipper a -> a
current (Z _ c _) = c

moveLeft :: Zipper a -> Zipper a
moveLeft (Z (l:ls) c rs) = Z ls l (c:rs)
moveLeft _ = error "The unthinkable happened"

moveRight :: Zipper a -> Zipper a
moveRight (Z ls c (r:rs)) = Z (c:ls) r rs
moveRight _ = error "The unthinkable happened"

flatten :: Int -> Zipper a -> [a]
-- Intentionally flattened like this to remove an unnecessary reverse.
flatten n (Z l c r) = c:(take n l ++ take n r)

newtype Node = N { step :: Machine -> Machine }

buildState :: (Zipper Bool -> Zipper Bool)
           -> Node
           -> (Zipper Bool -> Zipper Bool)
           -> Node
           -> Node
buildState mod0 to0 mod1 to1 = N $ \M {..} ->
  if current tape
    then M { tape = mod1 tape
           , state = to1
           , steps = steps + 1 }
    else M { tape = mod0 tape
           , state = to0
           , steps = steps + 1 }

diagnostics :: Int
diagnostics = 12667664

stateA, stateB, stateC, stateD, stateE, stateF :: Node
stateA = buildState (moveRight . set True)  stateB
                    (moveLeft . set False)  stateC
stateB = buildState (moveLeft . set True)   stateA
                    moveRight               stateD
stateC = buildState moveLeft                stateB
                    (moveLeft . set False)  stateE
stateD = buildState (moveRight . set True)  stateA
                    (moveRight . set False) stateB
stateE = buildState (moveLeft . set True)   stateF
                    moveLeft                stateC
stateF = buildState (moveRight . set True)  stateD
                    moveRight               stateA

initial :: Machine
initial = M { tape  = pure False
            , steps = 0
            , state = stateA }

solve :: Int -> Int
solve stop = length
           . filter id
           . flatten stop
           . tape
           . until ((== stop) . steps) f $ initial
  where f (m@M { state = N s }) = s m

main :: IO ()
main = print (solve diagnostics)
