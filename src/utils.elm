module Utils where

import Array exposing (Array)
import Model exposing (Board, Box)

getNWNeighbor : Int -> Int -> Board -> Maybe Box
getNWNeighbor index columns board =
  if index % columns == 0
  then Nothing
  else Array.get (index - columns - 1) board

getNNeighbor : Int -> Int -> Board -> Maybe Box
getNNeighbor index columns board =
  Array.get (index - columns) board

getNENeighbor : Int -> Int -> Board -> Maybe Box
getNENeighbor index columns board =
  if (index + 1) % columns == 0
  then Nothing
  else Array.get (index - columns + 1) board

getENeighbor : Int -> Int -> Board -> Maybe Box
getENeighbor index columns board =
  if (index + 1) % columns == 0
  then Nothing
  else Array.get (index + 1) board

getSENeighbor : Int -> Int -> Board -> Maybe Box
getSENeighbor index columns board =
  if (index + 1) % columns == 0
  then Nothing
  else Array.get (index + columns + 1) board

getSNeighbor : Int -> Int -> Board -> Maybe Box
getSNeighbor index columns board =
  Array.get (index + columns) board

getSWNeighbor : Int -> Int -> Board -> Maybe Box
getSWNeighbor index columns board =
  if index % columns == 0
  then Nothing
  else Array.get (index + columns - 1) board

getWNeighbor : Int -> Int -> Board -> Maybe Box
getWNeighbor index columns board =
  if index % columns == 0
  then Nothing
  else Array.get (index - 1) board

convertTo2DList : List Box -> Int -> Int -> List (List Box)
convertTo2DList boxList startIndex columns =
  let
    (currRow, remaining) = List.partition (\box -> box.index < startIndex + columns) boxList
  in
    if List.length currRow > 0
    then currRow :: (convertTo2DList remaining (startIndex + columns) columns)
    else []
