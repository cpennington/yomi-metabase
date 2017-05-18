module Yomi.EvolveMeta where

import Data.Ix (Ix)
import Control.Monad.Extra (iterateM)
import Control.Monad.Mersenne.Random (evalRandom)
import Data.Array (Array, listArray, range, (//), (!))
import Data.Function (on)
import Data.List (intercalate, groupBy, sort, nub, sortBy, nubBy, maximumBy, (\\))
import Data.Ord (comparing, Down(..))
import Data.Ratio (numerator, denominator)
import Numeric (showFFloat)
import System.Environment (getArgs)
import System.Random.Mersenne.Pure64 (newPureMT)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V
import Data.Maybe (fromJust)
import Control.Monad.Writer (Writer(..), tell, runWriter)
import Text.Printf (printf)

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

import Yomi.Types
import Yomi.WinRates

data Player = Player
    { blindPicks :: Array Character Double
    , counterPicks :: Array (Character, Character) Double
    } deriving (Show, Eq)

empty :: Ix i => (i, i) -> Array i Double
empty = flip listArray (repeat 0)

blindRow p = [blindPicks p ! char | char <- cast]
counterRow p c = [counterPicks p ! (c, c') | c' <- cast]

prettyPrintPlayer :: Player -> String
prettyPrintPlayer player = intercalate "\n" $ ("blind picks: " ++ blind) : counter
    where
        blind = formatRow $ blindRow player
        counter =
            [ (formatChar c) ++ (formatRow $ counterRow player c)
            | c <- cast
            ]
        formatChar char = (show char) ++ ": " ++ replicate (11 - length (show char)) ' '
        formatField (char, chance) = show char ++ ": " ++ (showFFloat (Just 10) chance) ""
        formatRow row = intercalate ", " $ map formatField $ sortBy (comparing (Down . snd)) $ filter ((>0) . snd) $ zip cast row


matchValue :: WinRate -> Player -> Player -> Integer -> Double
matchValue winRate player1 player2 requiredWins = sum
    [ chance1 * chance2 * value
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
                counterPick1Options =
                    [ (pickChance, pickIdx)
                    | char1' <- cast
                    , let pickChance = counterPicks player1 ! (char2, char1')
                    , let pickIdx = (char1', char2, win1, win2 - 1)
                    , pickChance > 0
                    ]
                counterPick1Msg = showFFloat (Just 3) (1 - rate) $ concat
                    [ " * sum("
                    , concatMap (\(c, i) -> show c ++ " * EV" ++ show i) counterPick1Options
                    , ")"
                    ]
                counterPick1 = sum $ map (\(c, i) -> c * (evs ! i)) counterPick1Options
                counterPick2Options =
                    [ (pickChance, pickArgs)
                    | char2' <- cast
                    , let pickChance = counterPicks player2 ! (char1, char2')
                    , let pickArgs = (char1, char2', win1 - 1, win2)
                    , pickChance > 0
                    ]
                counterPick2Msg = showFFloat (Just 3) rate $ concat
                    [ " * sum("
                    , concatMap (\(c, i) -> show c ++ " * EV" ++ show i) counterPick2Options
                    , ")"
                    ]
                counterPick2 = sum $ map (\(c, i) -> c * (evs ! i)) counterPick2Options
                rate = winRate char1 char2

        evs = listArray bounds
            [ev c1 c2 w1 w2 | (c1, c2, w1, w2) <- range bounds]
        bounds = ((Argagarg, Argagarg, 0, 0), (Zane, Zane, requiredWins, requiredWins))

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

evalMeta :: WinRate -> [Player] -> [Genome Bool] -> [Objective]
evalMeta winRate meta genomes = [sum [matchValue winRate c m 4 | m <- meta] | c <- players]
    where
        players = map decodePlayer genomes


selection popSize eliteSize = stochasticUniversalSampling (popSize - eliteSize)
crossover = twoPointCrossover 0.25
mutation = pointMutate 0.25
step winRate meta popSize eliteSize = nextGeneration Maximizing (evalMeta winRate $ map decodePlayer meta) (selection popSize eliteSize) eliteSize crossover mutation
initialize meta randCount = do
    randomized <- getRandomBinaryGenomes randCount (bitsPerBlind + (bitsPerCounter * numCounters * length cast))
    return $ meta ++ randomized

playerDist p1 p2 = sqrt $ sum $ zipWith (\x y -> (x - y)**2) (linear p1) (linear p2)
    where
        linear p = concat $ blindRow p : map (counterRow p) cast

playerToVect :: Player -> UV.Vector Double
playerToVect p = UV.fromList $ concat $ blindRow p : map (counterRow p) cast

runYear :: WinRate -> Int -> Int -> Int -> Int -> [Genome Bool] -> IO [(Genome Bool, Double)]
runYear winRate popSize eliteSize generations metaSize meta = do
    population <- runGA (initialize meta (popSize - length meta)) (loop (Generations generations) (step winRate meta popSize eliteSize))
    return $ topNMeta metaSize population

topNMeta metaSize population = take metaSize $ nubBy ((==) `on` (decodePlayer . fst)) $ bestFirst Maximizing $ population

clusterMeta metaSize population = take metaSize $ sortDescValue $ map (head . sortDescValue) $ fromJust $ clusters clustered
    where
        clustered = kmeansBy 10 (V.fromList population) (playerToVect . decodePlayer . fst) defaultKMeansOpts
        sortDescValue = sortBy (comparing (Down . snd))


meta20XX = Player
    (empty cast1D // [(Troq, 0.33333), (Zane, 0.33333), (Geiger, 0.33333)])
    (empty cast2D //
        [ ((Argagarg, Zane), 1.0)
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
    (empty cast2D //
        [ ((Argagarg, Zane), 1.0)
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
    let winRate = historicalWinRate

    putStrLn "grave20XX vs meta20XX"
    print $ matchValue winRate grave20XX meta20XX 4
    putStrLn ""

    let nextYear = runYear winRate popSize eliteSize generations metaSize
    let runForever = iterateM $ \meta -> do
            nextMeta <- nextYear meta

            putStrLn "New Meta"
            prettyPrintPop nextMeta

            putStrLn "Vs Old Metas"
            mapM (\oldMeta -> print $ map (\(p, _) -> matchValue winRate (decodePlayer p) (decodePlayer oldMeta) 4) nextMeta) meta

            return $ meta ++ map fst nextMeta

    runForever (map encodePlayer baselineMeta)