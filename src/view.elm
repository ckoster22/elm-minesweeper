module View where

import Html exposing (Html, Attribute, text, table, tr, td, div, button, h1)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Signal exposing (Address)
import Array exposing (Array)
import Actions exposing (Action)
import Model exposing (AppState, Box)
import Utils

view: Address Action -> AppState -> Html
view address model =
  let
    board2D = Utils.convertTo2DList (Array.toList model.board) 0 model.columns
  in
    div []
    [ h1 [] [ text "Minesweeper" ]
    , table [] (List.map (\boxRow -> rowView address boxRow) board2D)
    ]

rowView : Address Action -> List Box -> Html
rowView address boxes =
  tr [] (List.map (\box -> boxView address box) boxes)

boxView : Address Action -> Box -> Html
boxView address box =
  if box.isRevealed
  then
    case box.value of
      Model.Mine -> td [mineBoxStyle] [ text "M" ]
      Model.Clear neighbors -> td [boxStyleFor neighbors] [ toString neighbors |> text]
  else
    td [hiddenBoxStyle, onClick address (Actions.Reveal box)] []

boxStyleFor : Int -> Attribute
boxStyleFor neighbors =
  let
    color =
      case neighbors of
        1 -> "blue"
        2 -> "green"
        3 -> "red"
        4 -> "purple"
        5 -> "maroon"
        6 -> "aqua"
        otherwise -> "black"
  in
    style
      [ ("width", "30px")
      , ("height", "30px")
      , ("text-align", "center")
      , ("color", color)
      , ("font-weight", "bold")
      ]

hiddenBoxStyle : Attribute
hiddenBoxStyle =
  style
    [ ("width", "30px")
    , ("height", "30px")
    , ("text-align", "center")
    , ("cursor", "pointer")
    , ("background-color", "darkgray")
    ]

mineBoxStyle : Attribute
mineBoxStyle =
  style
    [ ("width", "30px")
    , ("height", "30px")
    , ("background-color", "#ff5050")
    , ("text-align", "center")
    , ("cursor", "pointer")
    ]
