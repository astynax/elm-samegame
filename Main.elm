module Main exposing (main)

import Blocks exposing (Matrix)
import Browser
import Collage as C exposing (Collage)
import Collage.Events as E
import Collage.Layout as L
import Collage.Render exposing (svgBox)
import Collage.Text as T
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
    = ClickAt ( Int, Int )
    | JustClick
    | NewGame Field


type alias Field =
    { blocks : Matrix Cell
    , width : Int
    , height : Int
    }


type Model
    = Generating
    | InProgress
        { field : Field
        , selection : Set ( Int, Int )
        }
    | GameIsOver
        { field : Field
        , score : Int
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
    ( Generating, newGame 20 20 )


newGame : Int -> Int -> Cmd Msg
newGame w h =
    Random.generate NewGame (fieldG w h)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame field ->
            ( InProgress
                { field = field
                , selection = Set.empty
                }
            , Cmd.none
            )

        JustClick ->
            case model of
                Generating ->
                    ( Generating, Cmd.none )

                GameIsOver { field } ->
                    ( Generating, newGame field.width field.height )

                InProgress m ->
                    ( InProgress { m | selection = Set.empty }, Cmd.none )

        ClickAt pos ->
            case model of
                InProgress ({ field } as m) ->
                    ( if Set.member pos m.selection then
                        if Set.size m.selection >= 2 then
                            let
                                new =
                                    Blocks.remove m.selection m.field.blocks

                                newField =
                                    { field | blocks = new }
                            in
                            if Blocks.hasAnyCluster new then
                                InProgress
                                    { m
                                        | field = newField
                                        , selection = Set.empty
                                    }

                            else
                                GameIsOver
                                    { field = newField
                                    , score = scoreFor newField
                                    }

                        else
                            model

                      else
                        InProgress
                            { m
                                | selection = Blocks.select pos m.field.blocks
                            }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


scoreFor : Field -> Int
scoreFor f =
    f.width * f.height - List.sum (List.map List.length f.blocks)


view : Model -> Html Msg
view model =
    svgBox ( 800, 800 ) <|
        case model of
            Generating ->
                L.empty

            InProgress { field, selection } ->
                viewField
                    selection
                    field.width
                    field.height
                    field.blocks

            GameIsOver { field, score } ->
                L.impose
                    (L.vertical
                        [ viewScore score
                        , restartButton
                        ]
                    )
                <|
                    viewField
                        Set.empty
                        field.width
                        field.height
                        field.blocks


viewScore : Int -> Collage a
viewScore s =
    T.fromString ("Score: " ++ String.fromInt s)
        |> T.size T.enormous
        |> T.color Color.yellow
        |> C.rendered


restartButton : Collage Msg
restartButton =
    L.impose
        (T.fromString "Restart"
            |> T.size T.enormous
            |> T.color Color.white
            |> C.rendered
        )
        (C.rectangle 120 50
            |> C.filled (C.uniform Color.blue)
        )
        |> E.onMouseDown (always JustClick)


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
                    ClickAt ( x, y )

                _ ->
                    JustClick
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


fieldG : Int -> Int -> Generator Field
fieldG w h =
    Random.map
        (\b ->
            { blocks = b
            , width = w
            , height = h
            }
        )
    <|
        Random.list w <|
            Random.list h <|
                Random.uniform R [ G, B ]


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
