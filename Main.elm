module Main exposing (main)

import Blocks exposing (Matrix)
import Browser
import Collage as C exposing (Collage)
import Collage.Events as E
import Collage.Layout as L
import Collage.Render exposing (svgBox)
import Color exposing (Color)
import Html exposing (Html)
import List
import Random exposing (Generator)
import Set exposing (Set)


type Cell
    = R
    | G
    | B


type Msg
    = Clicked ( Int, Int )
    | ClickedEmpty


type alias Model =
    { blocks : Matrix Cell
    , width : Int
    , height : Int
    , selection : Set ( Int, Int )
    }


main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( { blocks = mock <| field 20 20
      , width = 20
      , height = 20
      , selection = Set.empty
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    svgBox ( 800, 800 ) <|
        viewField
            model.selection
            model.width
            model.height
            model.blocks


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedEmpty ->
            ( { model | selection = Set.empty }
            , Cmd.none
            )

        Clicked pos ->
            ( if Set.member pos model.selection then
                if Set.size model.selection >= 2 then
                    { model
                        | blocks = Blocks.remove model.selection model.blocks
                        , selection = Set.empty
                    }

                else
                    model

              else
                { model
                    | selection = Blocks.select pos model.blocks
                }
            , Cmd.none
            )


viewField : Set ( Int, Int ) -> Int -> Int -> Matrix Cell -> Collage Msg
viewField s w h =
    L.center
        << L.horizontal
        << List.indexedMap (viewCol s h)
        << padUpTo w


viewCol : Set ( Int, Int ) -> Int -> Int -> Maybe (List Cell) -> Collage Msg
viewCol s h x =
    L.vertical
        << List.reverse
        << List.indexedMap (viewCell s x)
        << padUpTo h
        << Maybe.withDefault []


viewCell : Set ( Int, Int ) -> Int -> Int -> Maybe Cell -> Collage Msg
viewCell s x y mbCell =
    let
        block =
            C.filled
                (C.uniform <|
                    Maybe.withDefault Color.white <|
                        Maybe.map toColor mbCell
                )
            <|
                C.square 40

        dot =
            if Set.member ( x, y ) s then
                C.filled (C.uniform Color.white) (C.circle 10)

            else
                L.empty

        msg =
            case mbCell of
                Just _ ->
                    Clicked ( x, y )

                _ ->
                    ClickedEmpty
    in
    E.onMouseDown (always msg) <| L.impose dot block


toColor : Cell -> Color
toColor cell =
    case cell of
        R ->
            Color.red

        G ->
            Color.green

        B ->
            Color.blue


mock : Generator a -> a
mock g =
    Tuple.first <| Random.step g <| Random.initialSeed 42


field : Int -> Int -> Generator (List (List Cell))
field w h =
    Random.list w <| Random.list h <| Random.uniform R [ G, B ]


padUpTo : Int -> List a -> List (Maybe a)
padUpTo n l =
    let
        ll =
            List.length l

        front =
            List.map Just <| List.take (min n ll) l

        back =
            List.repeat (max 0 (n - ll)) Nothing
    in
    List.append front back
