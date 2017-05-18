module Yomi.Types where

import Data.Ix (Ix)
import Control.Monad.Writer (Writer)
import Data.List ((\\))

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

type WinRate = Character -> Character -> Double
type Logging = Writer [String]
type Wins = Int
type CounterPick = Character -> Wins -> Wins -> Logging [(Maybe Character, Double)]

cast = [Argagarg .. Zane]
cast19XX = cast \\ [DeGrey, Geiger, Troq, Zane]
cast1D = (Argagarg, Zane)
cast2D = ((Argagarg, Argagarg), (Zane, Zane))
