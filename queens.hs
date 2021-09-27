-- import Data.Functor.Foldable (hylo)
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, RecordWildCards #-}

import Control.Monad (guard)

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

--  1  2  3  4  5  6  7  8
--  Q
--        Q
--              Q
--                    Q
--           Q
--     Q
--                       Q
--                 Q

-- [1, 3, 5, 7, 4, 2, 8, 6]



data QueensF r = Done [Int] | Node [r] deriving (Functor, Show)

newtype Problem = Problem { previousQueenPositions :: [Int] } deriving Show

coAlg :: CoAlg QueensF Problem
coAlg Problem{..} = if length previousQueenPositions == 8 then
                      Done $ previousQueenPositions
                    else
                      Node $ do
                        possiblePosition <- [1..8]
                        guard $ isAvailableRow previousQueenPositions possiblePosition
                        return $ Problem (previousQueenPositions ++ [possiblePosition])
                        --[ Problem (previousQueenPositions ++ [possiblePosition]) |
                        --  possiblePosition <- [1..8], isAvailableRow previousQueenPositions possiblePosition
                        --]

type Solutions = [[Int]]


alg :: Alg QueensF Solutions
alg (Done solution) = [solution]
alg (Node severalSolutions) = concat severalSolutions

eightQueens :: Solutions
eightQueens = hylo alg coAlg $ Problem []

type UsedRows = [Int]

sameDiag (r1, c1) (r2, c2) = abs (r1 - r2) == abs (c1 - c2)

sameRow (r1, _) (r2, _) = r1 == r2

isAvailableRow :: UsedRows -> Int -> Bool
isAvailableRow rs r = let current = (r, length rs + 1)
                          previous = zip rs [1..]
                          ok prev = not (sameDiag prev current) && not (sameRow prev current)
                      in all ok previous

showQueens :: [Int] -> String
showQueens queens = unlines $ " 12345678" : map (\q-> replicate q ' ' ++ "Q") queens
