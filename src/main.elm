import Model exposing (AppState, Board, Box, BoxValue)
import Update
import Utils exposing (getNWNeighbor, getNNeighbor, getNENeighbor, getENeighbor, getSENeighbor, getSNeighbor, getSWNeighbor, getWNeighbor)

import View
import Array exposing (Array)
import Random exposing (generate, initialSeed, int, list, Seed)
import StartApp.Simple as StartApp

initialColumns = 30
initialRows = 16
initialMines = 99

appState : AppState
appState =
  { board = initBoard initialRows initialColumns initialMines
  , rows = initialRows
  , columns = initialColumns
  , mines = initialMines }

initBoard : Int -> Int -> Int -> Board
initBoard rows columns numMines =
  createEmptyBoard rows columns
    |> addMinesToBoard numMines
    |> populateNumbersForBoard columns

createEmptyBoard : Int -> Int -> Board
createEmptyBoard rows columns =
  Array.initialize (rows * columns) (\index -> defaultBoxAt index)

defaultBoxAt : Int -> Box
defaultBoxAt index =
  { isRevealed = False
  , value = Model.Clear 0
  , index = index }

addMinesToBoard : Int -> Board -> Board
addMinesToBoard numMines board =
  insertRandomMines board (initialSeed 80008) numMines

insertRandomMines : Board -> Seed -> Int -> Board
insertRandomMines board seed minesRemaining =
  let
    totalBoxes = Array.length board
    (randomIndex, seed') = generate (int 0 (totalBoxes - 1)) seed
    maybeCurrentBox = Array.get randomIndex board
  in
    case maybeCurrentBox of
      Just currentBox ->
        if currentBox.value == Model.Mine
        -- The box we were going to set to a mine is already a mine, try again
        then insertRandomMines board seed' minesRemaining
        else
          let
            -- Convert this box to a mine
            nextBoard = Array.set randomIndex { currentBox | value = Model.Mine } board
          in
            if minesRemaining > 1
            then insertRandomMines nextBoard seed' (minesRemaining - 1)
            else nextBoard
      Nothing -> board

populateNumbersForBoard : Int -> Board-> Board
populateNumbersForBoard columns board =
  Array.indexedMap (\index box -> if box.value == Model.Mine then box else populateNumbersForBoxAt index box board columns) board

populateNumbersForBoxAt : Int -> Box -> Board -> Int -> Box
populateNumbersForBoxAt index box board columns =
  let
    maybeNWBox = getNWNeighbor index columns board
    maybeNBox = getNNeighbor index columns board
    maybeNEBox = getNENeighbor index columns board
    maybeEBox = getENeighbor index columns board
    maybeSEBox = getSENeighbor index columns board
    maybeSBox = getSNeighbor index columns board
    maybeSWBox = getSWNeighbor index columns board
    maybeWBox = getWNeighbor index columns board

    numNeighborMines =
      maybeIncrementMineCount maybeNWBox
      + maybeIncrementMineCount maybeNBox
      + maybeIncrementMineCount maybeNEBox
      + maybeIncrementMineCount maybeEBox
      + maybeIncrementMineCount maybeSEBox
      + maybeIncrementMineCount maybeSBox
      + maybeIncrementMineCount maybeSWBox
      + maybeIncrementMineCount maybeWBox
  in
    { box | value = Model.Clear numNeighborMines }

maybeIncrementMineCount : Maybe Box -> Int
maybeIncrementMineCount maybeBox =
  case maybeBox of
    Just box ->
      if box.value == Model.Mine
      then 1
      else 0
    Nothing -> 0

main =
  StartApp.start { model = appState, view = View.view, update = Update.update }
