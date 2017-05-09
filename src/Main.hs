module Main where

import Data.Ix (Ix)
import Data.Array (Array, listArray, range, (//), (!))
import Data.List (intercalate)
import Numeric (showFFloat)
import System.Environment (getArgs)

import Moo.GeneticAlgorithm.Run (runGA, loop)
import Moo.GeneticAlgorithm.Types (Genome, Objective, ProblemType(..), Cond(..))
import Moo.GeneticAlgorithm.Binary (
    decodeGray
  , bitsNeeded
  , splitEvery
  , getRandomBinaryGenomes
  , twoPointCrossover
  , pointMutate
  , tournamentSelect
  , bestFirst
  , nextGeneration
  )

import Debug.Trace


data Character =
    Argagarg
  | Bbb
  | Degrey
  | Geiger
  | Gloria
  | Grave
  | Gwen
  | Jaina
  | Lum
  | Menelker
  | Midori
  | Onimaru
  | Persephone
  | Quince
  | Rook
  | Setsuki
  | Troq
  | Valerie
  | Vendetta
  | Zane
    deriving (Enum, Ix, Ord, Eq, Show)

cast = [Argagarg .. Zane]
cast1D = (Argagarg, Zane)
cast2D = ((Argagarg, Argagarg), (Zane, Zane))

data Player = Player {
    blindPicks :: Array Character Double
  , counterPicks :: Array (Character, Character) Double
  }
    deriving Show

prettyPrintPlayer :: Player -> String
prettyPrintPlayer player = intercalate "\n" $ ("blind picks: " ++ blind) : counter
    where
        blind = formatRow [blindPicks player ! char | char <- cast]
        counter = [
            (formatChar c) ++ (formatRow $ [counterPicks player ! (c, counter) | counter <- cast])
          | c <- cast
          ]
        formatChar char = (show char) ++ ": " ++ replicate (11 - length (show char)) ' '
        formatField (char, chance) = show char ++ ": " ++ (showFFloat (Just 2) chance) ""
        formatRow row = intercalate ", " $ map formatField $ filter ((>0) . snd) $ zip cast row

winRate = listArray cast2D [
{-Argagarg-}     5.00, 6.00, 4.75, 4.75, 3.50, 6.00, 5.75, 6.00, 6.50, 5.25, 4.75, 6.50, 4.25, 4.75, 4.75, 4.75, 3.75, 5.00, 4.75, 4.25
{-Bbb-}        , 4.00, 5.00, 4.50, 4.75, 4.00, 4.00, 4.50, 4.00, 4.00, 4.50, 6.50, 4.50, 6.00, 5.75, 6.25, 4.25, 5.75, 4.00, 4.50, 4.25
{-Degrey-}     , 5.25, 5.50, 5.00, 5.25, 6.00, 5.50, 6.00, 5.50, 6.00, 5.50, 6.00, 6.50, 4.75, 4.75, 5.25, 4.75, 4.50, 5.75, 5.25, 4.25
{-Geiger-}     , 5.25, 5.25, 4.75, 5.00, 4.50, 4.50, 4.75, 5.75, 5.25, 4.25, 6.50, 4.75, 4.00, 6.50, 6.00, 4.50, 6.00, 5.25, 5.25, 4.50
{-Gloria-}     , 6.50, 6.00, 4.00, 5.50, 5.00, 5.00, 5.00, 5.25, 4.25, 4.75, 4.75, 5.25, 4.50, 5.50, 4.75, 4.25, 4.25, 4.75, 5.50, 3.75
{-Grave-}      , 4.00, 6.00, 4.50, 5.50, 5.00, 5.00, 5.75, 5.75, 5.75, 5.00, 5.25, 5.25, 5.25, 4.75, 5.25, 5.50, 5.00, 5.75, 5.75, 5.50
{-Gwen-}       , 4.50, 5.50, 4.25, 5.50, 5.00, 4.25, 5.00, 5.00, 4.50, 5.50, 4.50, 4.50, 6.25, 4.75, 5.00, 4.75, 4.50, 4.00, 5.25, 4.50
{-Jaina-}      , 4.00, 6.00, 4.75, 4.25, 4.75, 4.50, 5.00, 5.00, 4.50, 4.50, 5.00, 4.50, 5.25, 5.50, 5.25, 4.00, 5.25, 5.25, 5.00, 4.50
{-Lum-}        , 3.50, 6.00, 4.00, 5.00, 5.75, 4.25, 5.50, 5.75, 5.00, 4.50, 5.75, 5.50, 4.75, 5.25, 6.00, 5.50, 6.00, 5.50, 6.25, 5.00
{-Menelker-}   , 4.75, 5.50, 4.50, 5.75, 5.25, 5.00, 4.50, 5.50, 5.50, 5.00, 4.50, 4.75, 5.50, 5.50, 4.75, 4.75, 4.25, 4.50, 5.25, 4.25
{-Midori-}     , 5.25, 3.50, 4.00, 3.50, 5.50, 4.75, 5.50, 5.00, 4.50, 5.50, 5.00, 4.25, 5.00, 4.25, 4.50, 5.50, 5.00, 5.00, 5.25, 4.25
{-Onimaru-}    , 3.50, 5.50, 3.50, 5.25, 4.75, 5.00, 5.50, 5.50, 4.50, 5.25, 5.75, 5.00, 4.50, 4.50, 5.75, 5.50, 5.75, 5.75, 5.25, 5.75
{-Persephone-} , 5.75, 4.00, 5.50, 6.00, 5.50, 4.75, 3.75, 4.75, 5.25, 4.50, 5.00, 5.50, 5.00, 5.25, 5.25, 4.25, 4.75, 4.50, 4.75, 4.00
{-Quince-}     , 5.25, 4.25, 5.25, 3.50, 4.75, 5.25, 5.25, 4.50, 4.75, 4.50, 5.75, 5.50, 4.75, 5.00, 5.25, 5.25, 4.75, 4.25, 4.50, 4.50
{-Rook-}       , 5.25, 3.75, 5.00, 4.00, 5.25, 4.75, 5.00, 4.75, 4.00, 5.50, 5.50, 4.50, 4.75, 4.75, 5.00, 4.75, 4.75, 4.50, 4.50, 5.50
{-Setsuki-}    , 5.25, 5.75, 5.50, 5.50, 6.00, 4.50, 5.25, 6.00, 4.50, 5.50, 4.50, 4.50, 5.75, 5.00, 5.25, 5.00, 4.25, 5.00, 5.50, 4.75
{-Troq-}       , 6.25, 4.50, 5.50, 4.00, 5.75, 5.00, 5.50, 4.75, 4.25, 5.75, 5.00, 4.25, 5.25, 5.25, 5.25, 5.75, 5.00, 5.50, 5.50, 6.25
{-Valerie-}    , 5.25, 6.00, 4.50, 4.75, 5.25, 4.25, 6.00, 4.75, 4.50, 5.50, 5.00, 4.25, 5.75, 5.75, 5.50, 5.00, 4.50, 5.00, 6.25, 5.00
{-Vendetta-}   , 5.25, 5.50, 4.75, 4.75, 4.50, 4.25, 4.75, 5.00, 3.75, 5.00, 4.75, 4.75, 5.25, 5.50, 5.50, 4.50, 4.50, 3.75, 5.00, 4.75
{-Zane-}       , 5.75, 5.75, 5.75, 5.50, 6.25, 4.50, 5.50, 5.50, 5.00, 5.75, 5.75, 4.25, 6.00, 5.75, 4.50, 5.25, 4.00, 5.00, 5.25, 5.00
  ]

matchValue player1 player2 requiredWins = sum [
    (blindPicks player1 ! c1) * (blindPicks player2 ! c2) * evs ! (c1, c2, requiredWins, requiredWins)
  | c1 <- cast
  , c2 <- cast
  ]
    where
        ev char1 char2 0 _ = 1
        ev char1 char2 _ 0 = 0
        ev char1 char2 win1 win2 = rate * counterPick2 + (1 - rate) * counterPick1
            where
                counterPick1 = sum [
                    pickChance * pickResult
                  | char1' <- cast
                  , let pickChance = counterPicks player1 ! (char2, char1')
                  , let pickResult = evs ! (char1', char2, win1, win2 - 1)
                  , pickChance > 0
                  ]
                counterPick2 = sum [
                    pickChance * pickResult
                  | char2' <- cast
                  , let pickChance = counterPicks player2 ! (char1, char2')
                  , let pickResult = evs ! (char1, char2', win1 - 1, win2)
                  , pickChance > 0
                  ]
                rate = winRate ! (char1, char2) / 10.0

        evs = listArray bounds
            [ev c1 c2 w1 w2 | (c1, c2, w1, w2) <- range bounds]
        bounds = ((Argagarg, Argagarg, 0, 0), (Zane, Zane, requiredWins, requiredWins))

empty :: Ix i => (i, i) -> Array i Double
empty = flip listArray (repeat 0)


chances = (0 :: Int, 127 :: Int)
bitsPerChance = bitsNeeded chances
characterRange = (fromEnum Argagarg, fromEnum Zane)
bitsPerChar = bitsNeeded characterRange
numCounters = 3 :: Int
numBlind = 10 :: Int
bitsPerCounter = (bitsPerChance + bitsPerChar) * numCounters
bitsPerBlind = (bitsPerChance + bitsPerChar) * numBlind

decodePlayer :: Genome Bool -> Player
decodePlayer genome = Player blinds counters
    where
        decodeRow numChars bits = zip cs ps
            where
                cs = map (toEnum . (flip mod $ fromEnum Zane) . decodeGray characterRange) $ splitEvery bitsPerChar $ take (bitsPerChar * numChars) bits
                ps = normalize $ map (fromIntegral . decodeGray chances) $ splitEvery bitsPerChance $ drop (bitsPerChance * numChars) bits

        normalize l = map (/  (max (sum l) 1)) l
        blindRow = decodeRow numBlind $ take bitsPerBlind genome
        counterRows = map (decodeRow numCounters) $ splitEvery bitsPerCounter $ drop bitsPerBlind genome
        blinds = empty cast1D // blindRow
        counters = empty cast2D // [((c1, c2), p) | (c1, ps) <- zip cast counterRows, (c2, p) <- ps]

roundRobinTournament :: [Genome Bool] -> [Objective]
roundRobinTournament genomes = [sum [ games ! (p1, p2) | p2 <- [1..playerCount]] | p1 <- [1..playerCount]]
    where
        playerCount = length genomes
        gameBounds = ((1, 1), (playerCount, playerCount))
        players = listArray (1, playerCount) $ map decodePlayer genomes
        play x y
            | x <= y = matchValue (players ! x) (players ! y) 4
            | otherwise = games ! (y, x)
        games = listArray gameBounds [play x y | (x, y) <- range gameBounds]

selection popSize eliteSize = tournamentSelect Maximizing 2 (popSize - eliteSize)
crossover = twoPointCrossover 0.25
mutation = pointMutate 0.25
step popSize eliteSize = nextGeneration Maximizing roundRobinTournament (selection popSize eliteSize) eliteSize crossover mutation
initialize popSize = getRandomBinaryGenomes popSize (bitsPerBlind + (bitsPerCounter * numCounters * length cast))


main = do
    args <- getArgs
    let [popSize, eliteSize, generations] = map read args
        roundRobinMatches = (fromIntegral ((popSize * (popSize + 1))) / 2.0) :: Float
        approxGamesPerMatch = 6 :: Int
        approxPathsPerMatch = ((numCounters * 2) ^ approxGamesPerMatch) * numBlind * numBlind
        totalPathsComputed = (roundRobinMatches * fromIntegral approxPathsPerMatch) * fromIntegral generations
    print totalPathsComputed
    population <- runGA (initialize popSize) (loop (Generations 100) (step popSize eliteSize))
    let (genome, objective) = head . bestFirst Maximizing $ population
    putStrLn $ prettyPrintPlayer $ decodePlayer genome
    print objective

-- main :: IO ()
-- main = print $ matchValue p1 p2 4
--     where
--         p1 = Player (empty cast1D // [(Argagarg, 1)]) (empty cast2D // [((c, Argagarg), 1) | c <- cast])
--         p2 = Player (empty cast1D // [(Grave, 1)]) (empty cast2D // [((c, Grave), 1) | c <- cast])