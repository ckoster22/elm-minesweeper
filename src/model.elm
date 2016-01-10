module Model where

import Array exposing (Array)

type alias Board = Array Box

type alias AppState =
  { board : Board
  , rows : Int
  , columns : Int
  , mines : Int }

type BoxValue = Mine | Clear Int
type alias Box =
  { isRevealed : Bool
  , value : BoxValue
  , index : Int}
