-- import Data.Functor.Foldable (hylo)

type Alg f e = f e -> e
type CoAlg f e = e -> f e

hylo :: Functor f => Alg f b -> CoAlg f a -> a -> b
hylo alg coalg = alg . fmap h . coalg
     where h = hylo alg coalg

--8                   Q    |
--7          Q             |
--6                      Q |
--5       Q                |
--4             Q          |
--3    Q                   |
--2             X  Q       |
--1 Q                      |

-- [1, 3, 5, 7, 4, 2, 8, 6]


type UsedRows = [Int]

sameDiag (r1, c1) (r2, c2) = abs (r1 - r2) == abs (c1 - c2)

sameRow (r1, _) (r2, _) = r1 == r2

isAvailableRow :: UsedRows -> Int -> Bool
isAvailableRow rs r = let current = (r, length rs + 1)
                          previous = zip rs [1..]
                          ok prev = not (sameDiag prev current) && not (sameRow prev current)
                      in all ok previous

queens :: [Int]
queens = undefined
