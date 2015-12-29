{-
  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Sheets where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal exposing (..)
import String 
import StartApp
import Maybe
import Result
import Cell
import Parse 
import Util exposing (..)
-- model 


initialModel : Model
initialModel = 
  { data = populateCells 20 20

  }


populateCells x y = 
  List.map (makeRow x) [0..y]
 

type alias Model =
  { data : List Row
  }


type alias Row =
  { row : List Cell.Model,
    id : Int
  } 


makeRow x y = 
  { id = y,
    row = List.map (Cell.makeEmpty y) [0..x]
  }


-- update
type Action 
  = Pass
  | PopulateCells
  | ToggleEditMode Cell.Id
  | ChangeCellContent Cell.Id String
  | UpdateCell Cell.Id


getCell : Model -> Cell.Id -> Maybe.Maybe Cell.Model
getCell model id =
  let 
    x = id.x
    y = id.y
  in
    model.data
      |> List.filter (\r -> r.id == y) -- get the right row == [Row]
      |> List.head -- filter retuns [Row], we want Row
      |> Maybe.map .row -- Row.row contains the list
      |> Maybe.map (List.drop x)
      |> Maybe.map List.head
      |> flattenMaybe 


update : Action -> Model -> Model
update action model =
  let 
    -- /=\ returns maybe

    --      |> Maybe.withDefault  (Cell.makeEmpty -1 -1)
    --uses a function to change the contents of a cell and return the 
    --edited model.data array
    editCell data pred id =
      let 
        findRow id row = 
          if row.id == id.y
            then 
              { row |
                row = List.map (findCell id.x) row.row
              }
            else row
        findCell x cell =
          if cell.id.x == x
            then pred cell -- modify cell
            else cell
      in 
        List.map (findRow id) data
    -- helper for editCell it will edit the cell and return the edited 
    -- model
    modifyCell model pred id = 
      { model | 
        data = editCell model.data pred id
      }
  in
    case action of
      Pass ->
        model

      PopulateCells ->
        { model |
          data = populateCells 20 20 
        }

      ToggleEditMode id ->
        let pred cell =
          { cell |
            editMode = not cell.editMode
          }
        in modifyCell model pred id

      ChangeCellContent id content ->
        let
          data = Parse.computeContent content (getCell model)
          pred cell = 
            { cell |
              content = content,
              data = fst data,
              refs = snd data
            }
        in modifyCell model pred id

      UpdateCell id ->
        let
          mayCell = getCell model id
          content = 
            case mayCell of
              Just cell ->
                cell.content
              Nothing ->
                ""
          data = Parse.computeContent content (getCell model)
          pred cell =
            { cell |
              data = fst data,
              refs = snd data
            }
        in modifyCell model pred id

-- view

onInput address f =
  on "input" targetValue (\v -> Signal.message address (f v))


cell : Address Action -> Cell.Model -> Html
cell address cellModel = 
  let
    label = 
      if cellModel.editMode == True
        then 
          input 
          [ placeholder cellModel.id.view, 
            value cellModel.content, 
            onBlur address (ToggleEditMode cellModel.id),
            onInput address (ChangeCellContent cellModel.id)
          ] [ ]
        else text cellModel.data
  in 
    td 
      [ onDoubleClick address (ToggleEditMode cellModel.id) ] 
      [ label ]


row : Address Action -> Row -> Html
row address rowModel =
  tr [ ] (List.map (cell address) rowModel.row)


header =
  let pred n = th [ ] [ text (toString (selectFromAlphabet n)) ]
  in List.map pred [0..20]


sheet : Address Action -> Model -> Html
sheet address model = 
  let 
    content = List.map (row address) model.data
  in
    table [ ] 
      [ thead [ ] (header ++ content) ]


view : Address Action -> Model -> Html
view address model = 
  div [ ] 
  [ button [ onClick address PopulateCells ] [ text (toString (Parse.convertCellName "c3")) ],
    sheet address model
  ]


main : Signal Html
main = 
  StartApp.start 
    { model = initialModel,
      view = view,
      update = update
    }
