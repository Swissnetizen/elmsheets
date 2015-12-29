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
module Cell where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Signal exposing (..)

import String 

import Util exposing (..)


type alias Model = 
  { id : Id,
    content : String,
    data : String,
    editMode: Bool,
    refs: List Id,
    refedBy: List Id
  }


type alias Id = 
  { view : String, -- "A1"
    x : Int, -- 0
    y : Int, -- 0
    xView : Char, -- 'A'
    yView : Char  -- '1'
  }

  
id : Int -> Int -> Id 
id x y =
  let 
    convertNo n = 
      n
        |> toString 
        |> String.uncons
        |> unsafeMaybe "id.convertNo"
        |> fst
    xView = selectFromAlphabet x
    yView = convertNo y
  in 
    { view = (toString xView) ++ (toString yView),
      x = x,
      y = y,
      xView = xView,
      yView = yView
    }


makeEmpty y x = 
  { id = id x y,
    content = "",
    data = "",
    editMode = False,
    refs = [],
    refedBy = []
  }


