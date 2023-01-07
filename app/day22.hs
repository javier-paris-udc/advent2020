module Main where


import           AoC                (applyInput, intP)
import           Control.Arrow      ((>>>))
import           Data.Foldable      (toList)
import           Data.Sequence      (Seq ((:<|), (:|>), Empty))
import qualified Data.Sequence      as Seq
import           Data.Set           (Set)
import qualified Data.Set           as Set
import           Text.Parsec        (newline, sepEndBy1, spaces, string)
import           Text.Parsec.String (Parser)


data Player = One | Two


recRounds :: Set (Seq Int, Seq Int) -> Seq Int -> Seq Int -> (Player, Seq Int)
recRounds playedRounds p1@(c1 :<| c1s) p2@(c2 :<| c2s)
    | (p1, p2) `Set.member` playedRounds = (One, p1)
    | Seq.length c1s >= c1 && Seq.length c2s >= c2 =
        case recRounds Set.empty (Seq.take c1 c1s) (Seq.take c2 c2s) of
            (Two, _) -> recRounds newSet c1s (c2s :|> c2 :|> c1)
            (One, _) -> recRounds newSet (c1s :|> c1 :|> c2) c2s
    | c1 < c2         = recRounds newSet c1s (c2s :|> c2 :|> c1)
    | otherwise       = recRounds newSet (c1s :|> c1 :|> c2) c2s
  where
    newSet = Set.insert (p1, p2) playedRounds
recRounds _ Empty p2    = (Two, p2)
recRounds _ p1    Empty = (One, p1)


rounds :: Seq Int -> Seq Int -> Seq Int
rounds Empty p2                  = p2
rounds p1    Empty               = p1
rounds (c1 :<| c1s) (c2 :<| c2s)
    | c1 < c2                    = rounds c1s (c2s :|> c2 :|> c1)
    | otherwise                  = rounds (c1s :|> c1 :|> c2) c2s


score :: Seq Int -> Int
score = toList
    >>> reverse
    >>> zipWith (*) [1..]
    >>> sum


solveP2 :: (Seq Int, Seq Int) -> Int
solveP2 = score . snd . uncurry (recRounds Set.empty)


solveP1 :: (Seq Int, Seq Int) -> Int
solveP1 = score . uncurry rounds


combatP :: Parser (Seq Int, Seq Int)
combatP = do
    player1 <- playerP
    _       <- newline
    player2 <- playerP
    return (player1, player2)
  where
    playerP = do
        _     <- string "Player "
        _     <- intP
        _     <- string ":"
        _     <- spaces
        cards <- intP `sepEndBy1` newline
        return $ Seq.fromList cards


main :: IO ()
main = applyInput combatP solveP1 solveP2