module Update where

import Array exposing (Array)
import Actions exposing (Action)
import Model exposing (AppState, Box, Board)
import Utils exposing (getNWNeighbor, getNNeighbor, getNENeighbor, getENeighbor, getSENeighbor, getSNeighbor, getSWNeighbor, getWNeighbor)

update : Action -> AppState -> AppState
update action currAppState =
  case action of
    Actions.Reveal box -> { currAppState | board = revealBox box currAppState.board currAppState.columns }

revealBox : Box -> Board -> Int -> Board
revealBox box board columns =
  case box.value of
    Model.Clear neighborMines ->
      if neighborMines == 0
      then recursivelyRevealEmptyNeighbors box board columns
      else Array.set box.index { box | isRevealed = True } board
    Model.Mine -> revealEntireBoard board

recursivelyRevealEmptyNeighbors : Box -> Board -> Int -> Board
recursivelyRevealEmptyNeighbors box board columns =
  let
    updatedBox = { box | isRevealed = True }
    maybeNWBox = getNWNeighbor updatedBox.index columns board
    maybeNBox = getNNeighbor updatedBox.index columns board
    maybeNEBox = getNENeighbor updatedBox.index columns board
    maybeEBox = getENeighbor updatedBox.index columns board
    maybeSEBox = getSENeighbor updatedBox.index columns board
    maybeSBox = getSNeighbor updatedBox.index columns board
    maybeSWBox = getSWNeighbor updatedBox.index columns board
    maybeWBox = getWNeighbor updatedBox.index columns board
  in
    Array.set box.index updatedBox board
      |> maybeExpandNeighbor maybeNWBox columns
      |> maybeExpandNeighbor maybeNBox columns
      |> maybeExpandNeighbor maybeNEBox columns
      |> maybeExpandNeighbor maybeEBox columns
      |> maybeExpandNeighbor maybeSEBox columns
      |> maybeExpandNeighbor maybeSBox columns
      |> maybeExpandNeighbor maybeSWBox columns
      |> maybeExpandNeighbor maybeWBox columns

-- If it's a box and is a "0" then reveal its neighboring boxes
maybeExpandNeighbor : Maybe Box -> Int -> Board -> Board
maybeExpandNeighbor maybeBox columns board =
  case maybeBox of
    Just box ->
      if not box.isRevealed
      then
        if box.value == Model.Clear 0
        then recursivelyRevealEmptyNeighbors box board columns
        else Array.set box.index { box | isRevealed = True } board
      else board
    Nothing -> board

revealEntireBoard : Board -> Board
revealEntireBoard board =
  Array.map (\box -> { box | isRevealed = True }) board
