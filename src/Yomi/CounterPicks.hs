module Yomi.CounterPicks where

import Data.Array (Array, listArray, range, (//), (!))
import Data.Maybe (fromJust)
import Data.List (intercalate, groupBy, sort, nub, sortBy, nubBy, maximumBy, (\\))
import Data.Ord (comparing, Down(..))
import Control.Monad.Writer (Writer(..), tell, runWriter)
import Text.Printf (printf)
import Data.Ix (Ix)
import Data.Map (Map, unionWith, empty)

import Yomi.Types
import Yomi.WinRates

optimalCounterPicks :: [Character] -> WinRate -> CounterPick
optimalCounterPicks cast winRate = (\c w w' -> evs ! (c, w, w'))
    where
        ev :: CounterPick
        ev _ 0 _ = return [(Nothing, 1)]
        ev _ _ 0 = return [(Nothing, 0)]
        ev opp winsReq oppWinsReq = do
                options <- mapM option cast
                let maxValue =  options
                log opp winsReq oppWinsReq maxValue
                return options
            where
                option cp = do
                    let rate = winRate cp opp
                    let (winValues, _) = runWriter $ evs ! (cp, oppWinsReq, winsReq - 1)
                    let (loseValues, _) = runWriter $ evs ! (opp, winsReq, oppWinsReq - 1)
                    return
                        ( Just cp
                        , rate * (1 - (snd $ maximumBy (comparing snd) winValues))
                        + (1 - rate) * (snd $ maximumBy (comparing snd) loseValues))

        log :: Character -> Wins -> Wins -> [(Maybe Character, Double)] -> Logging ()
        log opp winsReq oppWinsReq options = tell
            [ printf "EV%s = max(\n%s)"
                (show (opp, winsReq, oppWinsReq))
                (intercalate ",\n"
                    [ printf "  %.2f * (1 - EV%s) + %.2f * EV%s [%.2f%%]"
                        rate
                        (show (cp, oppWinsReq, winsReq - 1))
                        (1 - rate)
                        (show (cp, winsReq, oppWinsReq - 1))
                        (value * 100)
                    | (maybeChar, value) <- sortBy (comparing (Down . snd)) options
                    , let cp = fromJust maybeChar
                    , let rate = winRate cp opp
                    ]
                )
            ]
        evs = listArray bounds [ev c w w' | (c, w, w') <- range bounds]
        maxWins = 6
        bounds = ((Argagarg, 0, 0), (Zane, maxWins, maxWins))

matchFrequencies :: CounterPick -> Character -> Character -> Wins -> Wins -> Map (Character, Character)  Double
matchFrequencies _ _ _ 0 _ = empty
matchFrequencies _ _ _ _ 0 = empty
matchFrequencies pick c1 c2 w1 w2 = undefined -- unionWith (+) winFrequencies loseFrequencies
    where
        -- winFrequencies = map (* rate) $ matchFrequencies c1 c2' (w1-1) w2
        -- loseFrequencies = map (* (1-rate)) $ matchFrequencies c1' c2 w1 (w2-1)
        -- c2' = head $ pick c1 w2 (w1 - 1)
        -- c1' = head $ pick c2 w1 (w2 - 1)
        -- rate = undefined

prettyPrintCounterPicks
    :: CounterPick
    -> Character
    -> String
prettyPrintCounterPicks cp c = printf
    "[details=%s counterpicks]\n%s\n[/details]"
    (show c)
    (intercalate "\n" $ map snd $ sort
        [ ((Down $ snd $ head options), (printf " * %d-%d -> %s" w w' top3))
        | w <- [1..4]
        , w' <- [1..4]
        , let options = sortBy (comparing (Down . snd)) $ fst $ runWriter $ cp c w w'
        , let top3 = intercalate ", "
                [ printf "%s (%.1f%%)" (show $ fromJust char) (prob * 100)
                | (char, prob) <- take 3 options
                ]
        ])


main = do
    putStrLn "20XX Meta"

    putStrLn "Historical counterpicks\n"
    mapM_ (putStrLn . (prettyPrintCounterPicks $ optimalCounterPicks cast historicalWinRate)) cast

    putStrLn "Composite counterpicks\n"
    mapM_ (putStrLn . (prettyPrintCounterPicks $ optimalCounterPicks cast compositeWinRate)) cast

    putStrLn "19XX Meta"

    putStrLn "Historical counterpicks\n"
    mapM_ (putStrLn . (prettyPrintCounterPicks $ optimalCounterPicks cast19XX historicalWinRate)) cast19XX

    putStrLn "Composite counterpicks\n"
    mapM_ (putStrLn . (prettyPrintCounterPicks $ optimalCounterPicks cast19XX compositeWinRate)) cast19XX
