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
module Parse where

import String

import Maybe

import Util exposing (..)

import Cell

import Regex


computeContent : String -> (Cell.Id -> Maybe Cell.Model) -> (String, List Cell.Id)
computeContent content getCell = 
  let h = 
    String.slice 0 1 content
  in 
    if 
      | h == "=" -> computeForumla content getCell
      | otherwise -> (content, [])


coordRegex = Regex.regex "([A-Z][1-9]{1,3})"


computeForumla : String -> (Cell.Id -> Maybe Cell.Model) -> (String, List Cell.Id)
computeForumla f getCell = 
    let 
      processMaybe may = 
        case may of
          Just cell ->
            cell.data
          Nothing -> 
            "errol"

      pred c = 
        c
          |> .match
          |> convertCellName
          |> getCell
          |> processMaybe

      refs =
        f
        |> Regex.find Regex.All coordRegex
        |> List.map .match
        |> List.map convertCellName
    in 
      (Regex.replace Regex.All coordRegex pred f, refs)


convertCellName : String -> Cell.Id
convertCellName viewForm = 
  let 
    split =
      viewForm
        |> String.toUpper
        |> String.uncons
        |> unsafeMaybe "convert cell name split"
    predx e n = 
      if e /= (fst split) && (snd n) == False
        then ((fst n+1), False)
        else ((fst n), True)
    x = 
      alphabet
        |> List.foldl predx (0, False)
        |> fst
    y = 
      split
        |> snd 
        |> String.toInt
        |> Result.toMaybe
        |> unsafeMaybe "convert cell y"
        |> (\n -> n - 1) 
  in
    Cell.id x y
