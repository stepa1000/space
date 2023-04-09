module Data.Universe where

import Control.Comonad
import Control.Monad.Zip

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

universeNothing :: Universe (Maybe a)
universeNothing = makeUniverse id id Nothing

makeUniverseMaybe :: (a -> Maybe a) -> a -> Universe (Maybe a)
makeUniverseMaybe f a = makeUniverse (>>= f) (>>= f) (Just a)

makeUniverseM :: Monad m => (a -> m (Maybe a)) -> a -> m (Universe (Maybe a))
makeUniverseM f a = do
  midle <- f a
  ll <- l midle
  lr <- l midle
  return $ Universe ll midle lr
  where
    l (Just a2) = do
      ma <- (f a2)
      la <- l ma
      return $ ma : la
    l Nothing = return $ repeat Nothing

zipU :: Universe a -> Universe b -> Universe (a, b)
zipU (Universe la a ra) (Universe lb b rb) = Universe (mzip la lb) (a, b) (mzip ra rb)

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

makeUniverse2M :: Monad m => (a -> m (Maybe a)) -> a -> m (Universe2 (Maybe a))
makeUniverse2M f a = do
  midle <- makeUniverseM f a
  umum <- makeUniverseM (g . extract) midle
  return $ Universe2 $ fmap t umum
  where
    t (Just a) = a
    t Nothing = universeNothing
    g (Just _) = Just <$> makeUniverseM f a
    g a = return (Nothing)

zipU2 :: Universe2 a -> Universe2 b -> Universe2 (a, b)
zipU2 (Universe2 (Universe lu1 u1 ru1)) (Universe2 (Universe lu2 u2 ru2)) =
  Universe2 $ Universe (mzipWith zipU lu1 lu2) (zipU u1 u2) (mzipWith zipU ru1 ru2)

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
