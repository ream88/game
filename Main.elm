module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (..)


type Tile
    = Dirt
    | Air


type alias Row =
    Array Tile


type alias World =
    Array Row


tileToTexture : Tile -> String
tileToTexture tile =
    case tile of
        Dirt ->
            "dirt.png"

        Air ->
            ""


renderTile : Tile -> Html a
renderTile tile =
    let
        border =
            style
                [ ( "border", "1px solid grey" )
                , ( "width", "16px" )
                , ( "height", "16px" )
                , ( "background-image", "url(" ++ (tileToTexture tile) ++ ")" )
                ]
    in
        td [ border ] []


renderRow : Row -> Html a
renderRow tiles =
    tr [] (Array.map renderTile tiles |> Array.toList)


renderWorld : World -> Html a
renderWorld world =
    table [] (Array.map renderRow world |> Array.toList)



--


width : World -> Int
width world =
    world
        |> Array.get 0
        |> Maybe.map Array.length
        |> Maybe.withDefault 0


height : World -> Int
height world =
    Array.length world


generateWorld : Int -> Int -> World
generateWorld width height =
    Air
        |> Array.repeat width
        |> Array.repeat height


changeTile : Int -> Int -> Tile -> World -> World
changeTile x y tile world =
    world
        |> Array.get y
        |> Maybe.map (\row -> Array.set x tile row)
        |> Maybe.map (\row -> Array.set y row world)
        |> Maybe.withDefault world


changeRow : Int -> Int -> Int -> Tile -> World -> World
changeRow y x1 x2 tile world =
    List.range x1 x2
        |> Array.fromList
        |> Array.foldl (\x world -> changeTile x y tile world) world


changeColumn : Int -> Int -> Int -> Tile -> World -> World
changeColumn x y1 y2 tile world =
    List.range y1 y2
        |> Array.fromList
        |> Array.foldl (\y world -> changeTile x y tile world) world


makeBorder : World -> World
makeBorder world =
    world
        |> changeRow 0 0 (width world) Dirt
        |> changeRow ((height world) - 1) 0 (width world) Dirt
        |> changeColumn 0 0 (height world) Dirt
        |> changeColumn ((width world) - 1) 0 (height world) Dirt


makeEntry : World -> World
makeEntry =
    changeColumn 0 10 15 Air


makeExit : World -> World
makeExit world =
    changeColumn ((width world) - 1) 10 15 Air world


main : Html a
main =
    generateWorld 40 20
        |> makeBorder
        |> makeEntry
        |> makeExit
        |> renderWorld
