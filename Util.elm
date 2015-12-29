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
module Util where 

import String

import Maybe
import Debug

alphabet : List Char
alphabet = 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    |> String.toList 


selectFromAlphabet : Int -> Char
selectFromAlphabet n = 
      alphabet
        |> List.drop n
        |> List.head
        |> Maybe.withDefault '!'


unsafeMaybe : String -> Maybe.Maybe a -> a
unsafeMaybe error maybe = 
  case maybe of
    Just a ->
      a
    Nothing ->
      Debug.crash ("ERROR unsafeMaybe" ++ error)


flattenMaybe : Maybe (Maybe a) -> Maybe a
flattenMaybe m =
    case m of
        Just (Just value) -> Just value
        _ -> Nothing