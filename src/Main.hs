module Main where

import Control.Monad.Extra (iterateM)
import Control.Monad.Mersenne.Random (evalRandom)
import Data.Array (Array, listArray, range, (//), (!))
import Data.Function (on)
import Data.Ix (Ix)
import Data.List (intercalate, groupBy, sort, nub, sortBy, nubBy)
import Data.Ord (comparing, Down(..))
import Data.Ratio (numerator, denominator)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64 (newPureMT)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Data.Maybe (fromJust)

import Moo.GeneticAlgorithm.Run (runGA, loop)
import Moo.GeneticAlgorithm.Types (Genome, Objective, ProblemType(..), Cond(..))
import Moo.GeneticAlgorithm.Binary (
    decodeGray
  , encodeGray
  , bitsNeeded
  , splitEvery
  , getRandomBinaryGenomes
  , twoPointCrossover
  , pointMutate
  , tournamentSelect
  , stochasticUniversalSampling
  , bestFirst
  , nextGeneration
  )
import AI.Clustering.KMeans (kmeansBy, KMeans(..), defaultKMeansOpts)

import Debug.Trace


data Character =
    Argagarg
  | BBB
  | DeGrey
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
    deriving (Show, Eq)

blindRow p = [blindPicks p ! char | char <- cast]
counterRow p c = [counterPicks p ! (c, c') | c' <- cast]

prettyPrintPlayer :: Player -> String
prettyPrintPlayer player = intercalate "\n" $ ("blind picks: " ++ blind) : counter
    where
        blind = formatRow $ blindRow player
        counter = [
            (formatChar c) ++ (formatRow $ counterRow player c)
          | c <- cast
          ]
        formatChar char = (show char) ++ ": " ++ replicate (11 - length (show char)) ' '
        formatField (char, chance) = show char ++ ": " ++ (showFFloat (Just 10) chance) ""
        formatRow row = intercalate ", " $ map formatField $ sortBy (comparing (Down . snd)) $ filter ((>0) . snd) $ zip cast row

winRate = listArray cast2D [
{-               Arg, BBB, DeG, Gei, Glo, Gra, Gwe, Jai, Lum, Men, Mid, Oni, Per, Qui, Roo, Set, Tro, Val, Ven, Zan -}
{-Argagarg-}     5.0, 4.5, 4.9, 5.5, 4.2, 5.7, 5.6, 6.9, 6.8, 5.5, 6.2, 6.5, 5.8, 3.4, 5.8, 4.5, 4.5, 5.3, 5.8, 4.1
{-BBB-}        , 5.5, 5.0, 4.4, 3.1, 6.5, 4.0, 6.0, 5.2, 4.4, 4.4, 6.3, 4.6, 5.8, 6.2, 6.0, 4.6, 5.6, 6.1, 5.4, 4.2
{-DeGrey-}     , 5.1, 5.6, 5.0, 4.3, 4.9, 5.6, 4.9, 5.7, 6.8, 5.9, 4.5, 7.0, 5.5, 4.6, 4.9, 4.0, 4.5, 6.1, 4.4, 4.5
{-Geiger-}     , 4.5, 6.9, 5.7, 5.0, 6.9, 4.3, 4.8, 8.4, 5.5, 4.9, 6.2, 5.6, 4.4, 6.1, 5.5, 5.2, 6.1, 6.1, 5.1, 3.9
{-Gloria-}     , 5.8, 3.5, 5.1, 3.1, 5.0, 5.1, 4.3, 5.4, 4.7, 5.3, 3.3, 5.5, 5.5, 5.6, 6.2, 5.3, 3.9, 5.1, 5.0, 3.9
{-Grave-}      , 4.3, 6.0, 4.4, 5.7, 4.9, 5.0, 6.0, 6.0, 4.4, 4.8, 5.0, 5.1, 4.1, 3.7, 5.2, 4.5, 4.6, 5.3, 5.8, 4.4
{-Gwen-}       , 4.4, 4.0, 5.1, 5.3, 5.7, 4.0, 5.0, 4.8, 3.4, 5.3, 4.4, 3.7, 6.2, 5.1, 4.7, 4.0, 2.7, 4.8, 5.0, 3.9
{-Jaina-}      , 3.1, 4.8, 4.3, 1.6, 4.6, 4.0, 5.2, 5.0, 4.0, 4.2, 5.3, 3.4, 5.5, 6.8, 3.9, 4.3, 6.1, 6.7, 4.7, 2.9
{-Lum-}        , 3.3, 5.6, 3.2, 4.5, 5.3, 5.6, 6.6, 6.0, 5.0, 4.0, 4.8, 7.7, 7.3, 6.0, 5.6, 5.9, 5.4, 6.2, 4.9, 4.3
{-Menelker-}   , 4.5, 5.6, 4.1, 5.1, 4.7, 5.2, 4.7, 5.8, 6.0, 5.0, 4.9, 3.7, 6.1, 5.3, 4.2, 5.0, 3.5, 5.0, 5.3, 3.0
{-Midori-}     , 3.8, 3.8, 5.5, 3.8, 6.7, 5.0, 5.6, 4.7, 5.2, 5.1, 5.0, 4.4, 6.2, 4.7, 5.3, 4.4, 4.3, 3.9, 5.5, 5.3
{-Onimaru-}    , 3.5, 5.4, 3.0, 4.4, 4.5, 4.9, 6.3, 6.6, 2.3, 6.3, 5.6, 5.0, 4.8, 3.6, 4.9, 5.3, 5.5, 5.1, 6.2, 5.0
{-Persephone-} , 4.2, 4.2, 4.5, 5.6, 4.5, 5.9, 3.8, 4.5, 2.7, 3.9, 3.8, 5.2, 5.0, 5.3, 5.3, 3.1, 4.6, 3.2, 4.5, 3.0
{-Quince-}     , 6.6, 3.8, 5.4, 3.9, 4.4, 6.3, 4.9, 3.2, 4.0, 4.7, 5.3, 6.4, 4.7, 5.0, 4.5, 5.3, 3.0, 4.8, 5.0, 4.0
{-Rook-}       , 4.2, 4.0, 5.1, 4.5, 3.8, 4.8, 5.3, 6.1, 4.4, 5.8, 4.7, 5.1, 4.7, 5.5, 5.0, 4.5, 4.9, 5.6, 5.4, 5.7
{-Setsuki-}    , 5.5, 5.4, 6.0, 4.8, 4.7, 5.5, 6.0, 5.7, 4.1, 5.0, 5.6, 4.7, 6.9, 4.7, 5.5, 5.0, 3.8, 5.8, 5.9, 4.4
{-Troq-}       , 5.5, 4.4, 5.5, 3.9, 6.1, 5.4, 7.3, 3.9, 4.6, 6.5, 5.7, 4.5, 5.4, 7.0, 5.1, 6.2, 5.0, 6.0, 4.6, 6.5
{-Valerie-}    , 4.7, 3.9, 3.9, 3.9, 4.9, 4.7, 5.2, 3.3, 3.8, 5.0, 6.1, 4.9, 6.8, 5.2, 4.4, 4.3, 4.0, 5.0, 7.1, 4.1
{-Vendetta-}   , 4.2, 4.6, 5.6, 4.9, 5.0, 4.2, 5.0, 5.3, 5.1, 4.7, 4.5, 3.8, 5.5, 5.0, 4.6, 4.1, 5.4, 2.9, 5.0, 3.7
{-Zane-}       , 5.9, 5.8, 5.5, 6.1, 6.1, 5.6, 6.1, 7.1, 5.7, 7.0, 4.7, 5.0, 7.0, 6.0, 4.3, 5.6, 3.5, 5.9, 6.3, 5.0
-- {-Argagarg-}     5.00, 6.00, 4.75, 4.75, 3.50, 6.00, 5.75, 6.00, 6.50, 5.25, 4.75, 6.50, 4.25, 4.75, 4.75, 4.75, 3.75, 5.00, 4.75, 4.25
-- {-BBB-}        , 4.00, 5.00, 4.50, 4.75, 4.00, 4.00, 4.50, 4.00, 4.00, 4.50, 6.50, 4.50, 6.00, 5.75, 6.25, 4.25, 5.75, 4.00, 4.50, 4.25
-- {-DeGrey-}     , 5.25, 5.50, 5.00, 5.25, 6.00, 5.50, 6.00, 5.50, 6.00, 5.50, 6.00, 6.50, 4.75, 4.75, 5.25, 4.75, 4.50, 5.75, 5.25, 4.25
-- {-Geiger-}     , 5.25, 5.25, 4.75, 5.00, 4.50, 4.50, 4.75, 5.75, 5.25, 4.25, 6.50, 4.75, 4.00, 6.50, 6.00, 4.50, 6.00, 5.25, 5.25, 4.50
-- {-Gloria-}     , 6.50, 6.00, 4.00, 5.50, 5.00, 5.00, 5.00, 5.25, 4.25, 4.75, 4.75, 5.25, 4.50, 5.50, 4.75, 4.25, 4.25, 4.75, 5.50, 3.75
-- {-Grave-}      , 4.00, 6.00, 4.50, 5.50, 5.00, 5.00, 5.75, 5.75, 5.75, 5.00, 5.25, 5.25, 5.25, 4.75, 5.25, 5.50, 5.00, 5.75, 5.75, 5.50
-- {-Gwen-}       , 4.50, 5.50, 4.25, 5.50, 5.00, 4.25, 5.00, 5.00, 4.50, 5.50, 4.50, 4.50, 6.25, 4.75, 5.00, 4.75, 4.50, 4.00, 5.25, 4.50
-- {-Jaina-}      , 4.00, 6.00, 4.75, 4.25, 4.75, 4.50, 5.00, 5.00, 4.50, 4.50, 5.00, 4.50, 5.25, 5.50, 5.25, 4.00, 5.25, 5.25, 5.00, 4.50
-- {-Lum-}        , 3.50, 6.00, 4.00, 5.00, 5.75, 4.25, 5.50, 5.75, 5.00, 4.50, 5.75, 5.50, 4.75, 5.25, 6.00, 5.50, 6.00, 5.50, 6.25, 5.00
-- {-Menelker-}   , 4.75, 5.50, 4.50, 5.75, 5.25, 5.00, 4.50, 5.50, 5.50, 5.00, 4.50, 4.75, 5.50, 5.50, 4.75, 4.75, 4.25, 4.50, 5.25, 4.25
-- {-Midori-}     , 5.25, 3.50, 4.00, 3.50, 5.50, 4.75, 5.50, 5.00, 4.50, 5.50, 5.00, 4.25, 5.00, 4.25, 4.50, 5.50, 5.00, 5.00, 5.25, 4.25
-- {-Onimaru-}    , 3.50, 5.50, 3.50, 5.25, 4.75, 5.00, 5.50, 5.50, 4.50, 5.25, 5.75, 5.00, 4.50, 4.50, 5.75, 5.50, 5.75, 5.75, 5.25, 5.75
-- {-Persephone-} , 5.75, 4.00, 5.50, 6.00, 5.50, 4.75, 3.75, 4.75, 5.25, 4.50, 5.00, 5.50, 5.00, 5.25, 5.25, 4.25, 4.75, 4.50, 4.75, 4.00
-- {-Quince-}     , 5.25, 4.25, 5.25, 3.50, 4.75, 5.25, 5.25, 4.50, 4.75, 4.50, 5.75, 5.50, 4.75, 5.00, 5.25, 5.25, 4.75, 4.25, 4.50, 4.50
-- {-Rook-}       , 5.25, 3.75, 5.00, 4.00, 5.25, 4.75, 5.00, 4.75, 4.00, 5.50, 5.50, 4.50, 4.75, 4.75, 5.00, 4.75, 4.75, 4.50, 4.50, 5.50
-- {-Setsuki-}    , 5.25, 5.75, 5.50, 5.50, 6.00, 4.50, 5.25, 6.00, 4.50, 5.50, 4.50, 4.50, 5.75, 5.00, 5.25, 5.00, 4.25, 5.00, 5.50, 4.75
-- {-Troq-}       , 6.25, 4.50, 5.50, 4.00, 5.75, 5.00, 5.50, 4.75, 4.25, 5.75, 5.00, 4.25, 5.25, 5.25, 5.25, 5.75, 5.00, 5.50, 5.50, 6.25
-- {-Valerie-}    , 5.25, 6.00, 4.50, 4.75, 5.25, 4.25, 6.00, 4.75, 4.50, 5.50, 5.00, 4.25, 5.75, 5.75, 5.50, 5.00, 4.50, 5.00, 6.25, 5.00
-- {-Vendetta-}   , 5.25, 5.50, 4.75, 4.75, 4.50, 4.25, 4.75, 5.00, 3.75, 5.00, 4.75, 4.75, 5.25, 5.50, 5.50, 4.50, 4.50, 3.75, 5.00, 4.75
-- {-Zane-}       , 5.75, 5.75, 5.75, 5.50, 6.25, 4.50, 5.50, 5.50, 5.00, 5.75, 5.75, 4.25, 6.00, 5.75, 4.50, 5.25, 4.00, 5.00, 5.25, 5.00
  ]

matchValue :: Player -> Player -> Integer -> Double
matchValue player1 player2 requiredWins = sum [
    chance1 * chance2 * value
  | c1 <- cast
  , c2 <- cast
  , let chance1 = (blindPicks player1 ! c1)
  , let chance2 = (blindPicks player2 ! c2)
  , let value = evs ! (c1, c2, requiredWins, requiredWins)
  , chance1 > 0 && chance2 > 0
  ]
    where
        ev char1 char2 0 _ = 1
        ev char1 char2 _ 0 = 0
        ev char1 char2 win1 win2 = {-trace valueMsg-} value
            where
                valueMsg = "EV" ++ show (char1, char2, win1, win2) ++ " = " ++ (showFFloat (Just 3) value $ " = " ++ counterPick2Msg ++ " + " ++ counterPick1Msg)
                value = rate * counterPick2 + (1 - rate) * counterPick1
                counterPick1Options = [
                    (pickChance, pickIdx)
                  | char1' <- cast
                  , let pickChance = counterPicks player1 ! (char2, char1')
                  , let pickIdx = (char1', char2, win1, win2 - 1)
                  , pickChance > 0
                  ]
                counterPick1Msg = showFFloat (Just 3) (1 - rate) $ concat [
                    " * sum(", concatMap (\(c, i) -> show c ++ " * EV" ++ show i) counterPick1Options, ")"
                    ]
                counterPick1 = sum $ map (\(c, i) -> c * (evs ! i)) counterPick1Options
                counterPick2Options = [
                    (pickChance, pickArgs)
                  | char2' <- cast
                  , let pickChance = counterPicks player2 ! (char1, char2')
                  , let pickArgs = (char1, char2', win1 - 1, win2)
                  , pickChance > 0
                  ]
                counterPick2Msg = showFFloat (Just 3) rate $ concat [
                    " * sum(", concatMap (\(c, i) -> show c ++ " * EV" ++ show i) counterPick2Options, ")"
                    ]
                counterPick2 = sum $ map (\(c, i) -> c * (evs ! i)) counterPick2Options
                rate = winRate ! (char1, char2) / 10.0

        evs = listArray bounds
            [ev c1 c2 w1 w2 | (c1, c2, w1, w2) <- range bounds]
        bounds = ((Argagarg, Argagarg, 0, 0), (Zane, Zane, requiredWins, requiredWins))

empty :: Ix i => (i, i) -> Array i Double
empty = flip listArray (repeat 0)


chanceRange = (0 :: Int, 127 :: Int)
bitsPerChance = bitsNeeded chanceRange
characterRange = (fromEnum Argagarg, fromEnum Zane)
bitsPerChar = bitsNeeded characterRange
numCounters = 3 :: Int
numBlind = 10 :: Int
bitsPerCounter = (bitsPerChance + bitsPerChar) * numCounters
bitsPerBlind = (bitsPerChance + bitsPerChar) * numBlind

decodeRow :: Int -> Genome Bool -> [(Character, Double)]
decodeRow numChars bits = merged
    where
        merged = [
            (head chars, sum ps)
            | group <- groupedByChar
            , let (chars, ps) = unzip group
            ]
        groupedByChar = groupBy ((==) `on` fst) $ sort $ zip cs ps
        cs = map (toValidChar . decodeGray characterRange) charChunks
        ps = normalize $ map (fromIntegral . decodeGray chanceRange) probChunks

        charBits = take (bitsPerChar * numChars) bits
        charChunks = splitEvery bitsPerChar charBits
        toValidChar = toEnum . flip mod (fromEnum Zane)
        probBits = drop (bitsPerChar * numChars) bits
        probChunks = splitEvery bitsPerChance probBits

normalize l = map (/ (max (sum l) 1)) l

decodePlayer :: Genome Bool -> Player
decodePlayer genome = Player blinds counters
    where
        blindRow = decodeRow numBlind $ take bitsPerBlind genome
        counterRows = map (decodeRow numCounters) $ splitEvery bitsPerCounter $ drop bitsPerBlind genome
        blinds = empty cast1D // blindRow
        counters = empty cast2D // [((c1, c2), p) | (c1, ps) <- zip cast counterRows, (c2, p) <- ps]

encodeRow :: [(Character, Int)] -> [Bool]
encodeRow r = concat $ map (encodeGray characterRange . fromEnum) cs ++ map (encodeGray chanceRange) ps
    where
        (cs, ps) = unzip r

encodePlayer :: Player -> Genome Bool
encodePlayer player = genes
    where
        genes = blindGenes ++ counterGenes
        blindGenes = prepRow numBlind $ blindRow player
        counterGenes = concat $ map (prepRow numCounters) [counterRow player c | c <- cast]

        prepRow :: Int -> [Double] -> [Bool]
        prepRow n = encodeRow . take n . sortBy (comparing (Down . snd)) . zip cast . map (floor . (127 *))



evalMeta :: [Player] -> [Genome Bool] -> [Objective]
evalMeta meta genomes = [sum [matchValue c m 4 | m <- meta] | c <- players]
    where
        players = map decodePlayer genomes

selection popSize eliteSize = stochasticUniversalSampling (popSize - eliteSize)
crossover = twoPointCrossover 0.25
mutation = pointMutate 0.25
step meta popSize eliteSize = nextGeneration Maximizing (evalMeta $ map decodePlayer meta) (selection popSize eliteSize) eliteSize crossover mutation
initialize meta randCount = do
    randomized <- getRandomBinaryGenomes randCount (bitsPerBlind + (bitsPerCounter * numCounters * length cast))
    return $ meta ++ randomized

playerDist p1 p2 = sqrt $ sum $ zipWith (\x y -> (x - y)**2) (linear p1) (linear p2)
    where
        linear p = concat $ blindRow p : map (counterRow p) cast

playerToVect :: Player -> UV.Vector Double
playerToVect p = UV.fromList $ concat $ blindRow p : map (counterRow p) cast

runYear :: Int -> Int -> Int -> Int -> [Genome Bool] -> IO [(Genome Bool, Double)]
runYear popSize eliteSize generations metaSize meta = do
    population <- runGA (initialize meta (popSize - length meta)) (loop (Generations generations) (step meta popSize eliteSize))
    return $ topNMeta metaSize population

topNMeta metaSize population = take metaSize $ nubBy ((==) `on` (decodePlayer . fst)) $ bestFirst Maximizing $ population

clusterMeta metaSize population = take metaSize $ sortDescValue $ map (head . sortDescValue) $ fromJust $ clusters clustered
    where
        clustered = kmeansBy 10 (V.fromList population) (playerToVect . decodePlayer . fst) defaultKMeansOpts
        sortDescValue = sortBy (comparing (Down . snd))


meta20XX = Player
    (empty cast1D // [(Troq, 0.33333), (Zane, 0.33333), (Geiger, 0.33333)])
    (empty cast2D // [
        ((Argagarg, Zane), 1.0)
      , ((BBB, Geiger), 1.0)
      , ((DeGrey, Zane), 1.0)
      , ((Geiger, Zane), 1.0)
      , ((Gloria, Zane), 1.0)
      , ((Grave, Zane), 1.0)
      , ((Gwen, Troq), 1.0)
      , ((Jaina, Zane), 1.0)
      , ((Lum, DeGrey), 1.0)
      , ((Menelker, Zane), 1.0)
      , ((Midori, Geiger), 1.0)
      , ((Onimaru, DeGrey), 1.0)
      , ((Persephone, Zane), 1.0)
      , ((Quince, Troq), 1.0)
      , ((Rook, BBB), 1.0)
      , ((Setsuki, Troq), 1.0)
      , ((Troq, Geiger), 1.0)
      , ((Valerie, Zane), 1.0)
      , ((Vendetta, Zane), 1.0)
      , ((Zane, Troq), 1.0)
      ])

grave20XX = Player
    (empty cast1D // [(Grave, 1.0)])
    (empty cast2D // [
        ((Argagarg, Zane), 1.0)
      , ((BBB, Geiger), 1.0)
      , ((DeGrey, Zane), 1.0)
      , ((Geiger, Grave), 1.0)
      , ((Gloria, Zane), 1.0)
      , ((Grave, Zane), 1.0)
      , ((Gwen, Troq), 1.0)
      , ((Jaina, Zane), 1.0)
      , ((Lum, DeGrey), 1.0)
      , ((Menelker, Zane), 1.0)
      , ((Midori, Geiger), 1.0)
      , ((Onimaru, DeGrey), 1.0)
      , ((Persephone, Zane), 1.0)
      , ((Quince, Troq), 1.0)
      , ((Rook, BBB), 1.0)
      , ((Setsuki, Troq), 1.0)
      , ((Troq, Geiger), 1.0)
      , ((Valerie, Zane), 1.0)
      , ((Vendetta, Zane), 1.0)
      , ((Zane, Troq), 1.0)
    ])

prettyPrintPop pop = mapM pp pop
    where
        pp (g, v) = do
            putStrLn $ prettyPrintPlayer $ decodePlayer g
            print v

main = do
    args <- getArgs
    let [popSize, eliteSize, generations, metaSize] = map read args
        matches = popSize * metaSize
        approxGamesPerMatch = 6 :: Int
        approxPathsPerMatch = ((numCounters * 2) ^ approxGamesPerMatch) * numBlind * numBlind
        totalPathsComputed = (matches * fromIntegral approxPathsPerMatch) * fromIntegral generations
    print totalPathsComputed
    let baselineMeta = [meta20XX, grave20XX]

    putStrLn "grave20XX vs meta20XX"
    print $ matchValue grave20XX meta20XX 4
    putStrLn ""

    let nextYear = runYear popSize eliteSize generations metaSize
    let runForever = iterateM $ \meta -> do
            nextMeta <- nextYear meta

            putStrLn "New Meta"
            prettyPrintPop nextMeta

            putStrLn "Vs Old Metas"
            mapM (\oldMeta -> print $ map (\(p, _) -> matchValue (decodePlayer p) (decodePlayer oldMeta) 4) nextMeta) meta

            return $ meta ++ map fst nextMeta

    runForever (map encodePlayer baselineMeta)

-- main :: IO ()
-- main = print $ matchValue p1 p2 4
--     where
--         p1 = Player (empty cast1D // [(Argagarg, 1)]) (empty cast2D // [((c, Argagarg), 1) | c <- cast])
--         p2 = Player (empty cast1D // [(Grave, 1)]) (empty cast2D // [((c, Grave), 1) | c <- cast])