module Data.Universe where

import Control.Comonad

data Universe a = Universe [a] a [a]

newtype Universe2 a = Universe2 {getUniverse2 :: Universe (Universe a)}

left :: Universe a -> Universe a
left (Universe (a : as) x bs) = Universe as a (x : bs)

right :: Universe a -> Universe a
right (Universe as x (b : bs)) = Universe (x : as) b bs

goto :: Int -> Universe a -> Universe a
goto i u
  | i > 0 = goto (i - 1) $ right u
  | i < 0 = goto (i + 1) $ left u
  | True = u

makeUniverse fl fr x = Universe (tail $ iterate fl x) x (tail $ iterate fr x)

instance Functor Universe where
  fmap f (Universe as x bs) = Universe (fmap f as) (f x) (fmap f bs)

instance Comonad Universe where
  duplicate = makeUniverse left right
  extract (Universe _ x _) = x

takeRange :: (Int, Int) -> Universe a -> [a]
takeRange (a, b) u = take (b - a + 1) x
  where
    Universe _ _ x
      | a < 0 = iterate left u !! (-a + 1)
      | otherwise = iterate right u !! (a - 1)

instance Functor Universe2 where
  fmap f = Universe2 . (fmap . fmap) f . getUniverse2

instance Comonad Universe2 where
  extract = extract . extract . getUniverse2
  duplicate = fmap Universe2 . Universe2 . shifted . shifted . getUniverse2
    where
      shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
      shifted = makeUniverse (fmap left) (fmap right)

up :: Universe2 a -> Universe2 a
up (Universe2 u) = Universe2 $ fmap right u

down :: Universe2 a -> Universe2 a
down (Universe2 u) = Universe2 $ fmap left u

left2 :: Universe2 a -> Universe2 a
left2 (Universe2 u) = Universe2 $ left u

right2 :: Universe2 a -> Universe2 a
right2 (Universe2 u) = Universe2 $ right u

goto2 :: Int -> Int -> Universe2 a -> Universe2 a
goto2 x y (Universe2 u) = Universe2 $ goto x $ fmap (goto y) u

mapgoto2 :: Int -> Int -> (Universe2 a -> Universe2 a) -> Universe2 a -> Universe2 a
mapgoto2 x y f = goto2 (-x) (-y) . f . goto2 x y

takeRange2 :: (Int, Int) -> (Int, Int) -> Universe2 a -> [[a]]
takeRange2 (x0, y0) (x1, y1) =
  takeRange (y0, y1)
    . fmap (takeRange (x0, x1))
    . getUniverse2

takeRange2Box :: Int -> Universe2 a -> [[a]]
takeRange2Box i = takeRange2 (-i, -i) (i, i)
