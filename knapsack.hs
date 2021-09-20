{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
-- import Data.Functor.Foldable (hylo)

type Alg f e = f e -> e
type CoAlg f e = e -> f e

hylo :: Functor f => Alg f b -> CoAlg f a -> a -> b
hylo alg coalg = alg . fmap h . coalg
     where h = hylo alg coalg

-- tenemos capacidad de mochila
-- dado unos objetos con peso w y valor v
-- maximizar v

newtype Weight = Weight Int deriving (Show, Num, Eq, Ord)
newtype Value = Value Int deriving (Show, Num, Eq, Ord)

type Object = (Value, Weight)

data KnapSackF r = Done | Fits r (Object, r) | DoesntFit r deriving Functor

data Problem = Problem { capacity :: Weight, pendingObjects :: [Object] } deriving (Show)

coAlg :: CoAlg KnapSackF Problem
coAlg p = case pendingObjects p of
            [] -> Done
            (o@(v, w):os) -> if capacity p - w < 0 then
                                DoesntFit dropP
                             else
                                Fits dropP (o, takesP)
                             where takesP = Problem { capacity = (capacity p - w), pendingObjects = os }
                                   dropP = (p { pendingObjects = os })

data Solution = Solution { value :: Value, objects :: [Object] } deriving (Show)

alg :: Alg KnapSackF Solution
alg Done = Solution { value = 0, objects = [] }
alg (DoesntFit s) = s
alg (Fits s1 (o@(v, w), s2)) = if value s1 > value takenSolution then
                                  s1
                               else
                                  takenSolution
                               where takenSolution = s2 { value = value s2 + v, objects = o : objects s2 }
--alg (Fits s1 (o@(v, w), s2)) = takenSolution
--                               where takenSolution = s2 { value = value s2 + v, objects = o : objects s2 }

findObjectsMaxValue = hylo alg coAlg

main = print $ findObjectsMaxValue $ Problem 11 [(1, 1), (6, 10), (3, 5), (3, 5)]
