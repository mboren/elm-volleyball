module Grid exposing (..)

{-| This module was made for UI elements that will be laid out in a grid.
and can span multiple rows and columns. It could be used for things other
than UI, but I've mostly only implemented functionality that I need for
the volleyball project, so it's not fully generic.
For example, the functions here assume that you will only want to move to
the right and down while laying out your grid.

Each grid has a cursor which controls where new items will be placed
and how big they will be. The cursor manipulation functions provide a
little DSL for adding things to a grid.
For example:

    grid : Grid String
    grid
        |> setHeight 1
        |> setWidth 5
        |> insert "A"
        |> insert "B"
        |> nextRow
        |> setWidth 10
        |> insert "C"

results in something like this:
+-----+-----+
|..A..|..B..|
+-----+-----+
|.....C.....|
+-----------+
-}


type alias Row =
    Int


type alias Col =
    Int


type alias Grid a =
    { config : Config
    , data : List ( Region, a )
    , cursor : Region
    , startCol : Col
    }


type alias Region =
    { row : Row
    , col : Col
    , w : Int
    , h : Int
    }


type alias Cursor b =
    { b
        | cursor : Region
    }


type alias Config =
    { rows : Int
    , cols : Int
    , rowPadding : Float
    , height : Float
    , width : Float
    , xOffset : Float
    , yOffset : Float
    }


create : Config -> Grid a
create config =
    { config = config
    , data = []
    , cursor = { row = 0, col = 0, w = 1, h = 1 }
    , startCol = 0
    }


map : (a -> b) -> Grid a -> Grid b
map f grid =
    { grid | data = List.map (Tuple.mapSecond f) grid.data }


-- Functions for manipulating cursor and inserting new regions
-- These are all designed to work well in a pipeline.


{-| Insert a cell at cursor location and move right.
-}
insert : a -> Grid a -> Grid a
insert newCellData grid =
    { grid | data = ( grid.cursor, newCellData ) :: grid.data }
        |> goRight

{-| Insert a cell at cursor location without advancing cursor
-}
insertBackground : a -> Grid a -> Grid a
insertBackground newCellData grid =
    { grid | data = ( grid.cursor, newCellData ) :: grid.data }

{-| Change cursor width
-}
setWidth : Int -> Cursor a -> Cursor a
setWidth w curs =
    let
        oldCursor =
            curs.cursor
    in
    { curs | cursor = { oldCursor | w = w } }


{-| Change cursor height
-}
setHeight : Int -> Cursor a -> Cursor a
setHeight h curs =
    let
        oldCursor =
            curs.cursor
    in
    { curs | cursor = { oldCursor | h = h } }


{-| Move cursor right without inserting anything
-}
goRight : Cursor a -> Cursor a
goRight curs =
    let
        oldCursor =
            curs.cursor
    in
    { curs | cursor = { oldCursor | col = oldCursor.col + oldCursor.w } }

{-| Move cursor down one row and move to startCol
-}
nextRow : Grid a -> Grid a
nextRow grid =
    let
        oldCursor =
            grid.cursor
    in
    { grid
        | cursor =
            { oldCursor
                | col = grid.startCol
                , row = oldCursor.row + oldCursor.h
            }
    }

{-| Move cursor down one row and move to startCol
-}
prevRow : Grid a -> Grid a
prevRow grid =
    let
        oldCursor =
            grid.cursor
    in
    { grid
        | cursor =
            { oldCursor
                | col = grid.startCol
                , row = oldCursor.row - oldCursor.h
            }
    }

{-| Create a 1 row gap. Useful for separating sections by an amount
larger than rowPadding
-}
nextSection : Grid a -> Grid a
nextSection grid =
    grid
        |> nextRow
        |> setHeight 1
        |> nextRow


{-| Calling this will make nextRow start at this column the next time it is called
It's like setting the indentation level for UI elements.
-}
markAsStartCol : Grid a -> Grid a
markAsStartCol grid =
    { grid | startCol = grid.cursor.col }


resetStartCol : Grid a -> Grid a
resetStartCol grid =
    { grid | startCol = 0 }



-- Conversion from grid coordinates to screen coordinates


rowHeight : Config -> Float
rowHeight { rows, rowPadding, height } =
    height / toFloat rows - rowPadding


colWidth : Config -> Float
colWidth { cols, width } =
    width / toFloat cols


regionHeight : Config -> Region -> Float
regionHeight { rows, rowPadding, height } region =
    toFloat region.h * (height / toFloat rows) - rowPadding


{-| x coordinate of left edge of column
-}
x : Config -> Col -> Float
x grid col =
    grid.xOffset + toFloat col * colWidth grid


{-| y coordinate of top edge of row
-}
y : Config -> Row -> Float
y grid row =
    grid.yOffset + toFloat row * (grid.rowPadding + rowHeight grid)


{-| Create a rectangular path for a region.
Negative x values are set to zero.
-}
regionToPath : Config -> Region -> List ( Float, Float )
regionToPath cf reg =
    let
        xi =
            x cf reg.col

        yi =
            y cf reg.row

        w =
            toFloat reg.w * colWidth cf

        h =
            regionHeight cf reg
    in
    [ ( max 0 xi, yi )
    , ( max 0 (xi + w), yi )
    , ( max 0 (xi + w), yi + h )
    , ( max 0 xi, yi + h )
    ]


{-| Change x values in a path to skew everything
horizontally by the given slope.
Turns rectangles into parallelograms.
-}
skewPath : Float -> List ( Float, Float ) -> List ( Float, Float )
skewPath slope path =
    path
        |> List.map
            (\( x, y ) ->
                ( if x == 0.0 then
                    0.0
                  else
                    x + y / slope
                , y
                )
            )


{-| Calculate the centroid of a path.
For rectangles and parallelograms, this is a reasonable place to put text.
-}
centroid : List ( Float, Float ) -> ( Float, Float )
centroid path =
    let
        ( xVals, yVals ) =
            List.unzip path

        avgX =
            List.sum xVals / toFloat (List.length xVals)

        avgY =
            List.sum yVals / toFloat (List.length yVals)
    in
    ( avgX, avgY )
