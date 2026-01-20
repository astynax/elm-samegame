module Main exposing (Cell(..), Msg, blocks, hearts, main, viewCell)

import Blocks exposing (Matrix)
import Browser
import Collage as C exposing (Collage)
import Collage.Events as E
import Collage.Layout as L
import Collage.Render exposing (svgExplicit)
import Collage.Text as T
import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import List
import Random exposing (Generator)
import Set exposing (Set)
import String


type Cell
    = C1
    | C2
    | C3


type Msg
    = ClickAt ( Int, Int )
    | JustClick
    | NewGame Field
    | NewThemeBase Float
    | NextTheme
    | ChangeThemeBase


type alias Field =
    { blocks : Matrix Cell
    , width : Int
    , height : Int
    }


type alias Model =
    { state : State
    , theme : Theme Msg
    , themeName : ThemeName
    , themeBase : Float
    , moves : List Int
    , score : Int
    }


type State
    = Generating
    | InProgress
        { field : Field
        , selection : Set ( Int, Int )
        }
    | GameIsOver
        { field : Field
        , score : Int
        }


type alias Theme a =
    { cell : Maybe Cell -> Float -> Collage a
    , mark : Float -> Collage a
    }


type ThemeName
    = Blocks
    | Hearts


main =
    Browser.element
        { init = \() -> init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : ( Model, Cmd Msg )
init =
    let
        base =
            0.2

        mkTheme =
            themeByName Hearts
    in
    ( { state = Generating
      , theme = mkTheme base
      , themeName = Hearts
      , themeBase = base
      , moves = []
      , score = 0
      }
    , newGame 20 20
    )


newGame : Int -> Int -> Cmd Msg
newGame w h =
    Random.generate NewGame <| fieldG w h


newThemeBase : Cmd Msg
newThemeBase =
    Random.generate NewThemeBase <| Random.float 0 0.333


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeThemeBase ->
            ( model, newThemeBase )

        NextTheme ->
            let
                tn =
                    nextThemeName model.themeName
            in
            ( { model
                | themeName = tn
                , theme = themeByName tn model.themeBase
              }
            , Cmd.none
            )

        NewThemeBase base ->
            ( { model
                | theme = themeByName model.themeName base
                , themeBase = base
              }
            , Cmd.none
            )

        NewGame field ->
            ( { model
                | state =
                    InProgress
                        { field = field
                        , selection = Set.empty
                        }
                , moves = []
                , score = 0
              }
            , Cmd.none
            )

        JustClick ->
            case model.state of
                Generating ->
                    ( model, Cmd.none )

                GameIsOver { field } ->
                    ( { model | state = Generating }
                    , newGame field.width field.height
                    )

                InProgress m ->
                    ( { model | state = InProgress { m | selection = Set.empty } }
                    , Cmd.none
                    )

        ClickAt pos ->
            case model.state of
                InProgress ({ field } as m) ->
                    ( if Set.member pos m.selection then
                        if Set.size m.selection >= 2 then
                            let
                                new =
                                    Blocks.remove m.selection m.field.blocks

                                newField =
                                    { field | blocks = new }
                            in
                            { model
                                | state =
                                    if Blocks.hasAnyCluster new then
                                        InProgress
                                            { m
                                                | field = newField
                                                , selection = Set.empty
                                            }

                                    else
                                        GameIsOver
                                            { field = newField
                                            , score = model.score
                                            }
                                , moves = Set.size m.selection :: model.moves
                                , score =
                                    let
                                        x =
                                            Set.size m.selection
                                    in
                                    model.score + x * x
                            }

                        else
                            model

                      else
                        { model
                            | state =
                                InProgress
                                    { m
                                        | selection = Blocks.select pos m.field.blocks
                                    }
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


nextThemeName : ThemeName -> ThemeName
nextThemeName name =
    case name of
        Blocks ->
            Hearts

        Hearts ->
            Blocks


themeByName : ThemeName -> (Float -> Theme Msg)
themeByName name =
    case name of
        Blocks ->
            blocks

        Hearts ->
            hearts


view : Model -> Html Msg
view model =
    Html.div
        [ HA.style "text-align" "center"
        ]
        [ Html.h1 []
            [ Html.strong [] [ Html.text "SameGame" ]
            , Html.em []
                [ Html.text " by "
                , Html.a [ HA.href "https://astynax.me" ]
                    [ Html.text "astynax" ]
                ]
            ]
        , Html.button
            [ HE.onClick NextTheme
            , HA.title "Change shape of pieces"
            ]
            [ Html.text "🧩" ]
        , Html.button
            [ HE.onClick ChangeThemeBase
            , HA.title "Change colors"
            ]
            [ Html.text "🎨" ]
        , Html.span [ HA.style "margin-left" "5px" ] [ Html.text <| String.append "Score: " <| String.fromInt model.score ]
        , Html.div [] [ viewGame model ]
        , Html.pre []
            [ Html.text <|
                String.join " " <|
                    List.map String.fromInt model.moves
            ]
        ]


viewGame : Model -> Html Msg
viewGame model =
    wrapSvg <|
        case model.state of
            Generating ->
                L.empty

            InProgress { field, selection } ->
                viewField
                    model.theme
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
                        model.theme
                        Set.empty
                        field.width
                        field.height
                        field.blocks


wrapSvg : Collage a -> Html a
wrapSvg =
    svgExplicit
        [ HA.width 800
        , HA.height 800
        , HA.attribute "version" "1.1"
        , HA.style "user-select" "none"
        , HA.style "-webkit-user-select" "none"
        , HA.style "cursor" "crosshair"
        ]
        << C.shift ( 400, -400 )


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


viewField : Theme Msg -> Set ( Int, Int ) -> Int -> Int -> Matrix Cell -> Collage Msg
viewField t s w h =
    L.center
        << L.horizontal
        << List.indexedMap (viewCol t s h)
        << padUpTo w


viewCol : Theme Msg -> Set ( Int, Int ) -> Int -> Int -> Maybe (List Cell) -> Collage Msg
viewCol t s h x =
    L.vertical
        << List.reverse
        << List.indexedMap (viewCell t s x)
        << padUpTo h
        << Maybe.withDefault []


viewCell : Theme Msg -> Set ( Int, Int ) -> Int -> Int -> Maybe Cell -> Collage Msg
viewCell theme s x y mbCell =
    let
        dot =
            if Set.member ( x, y ) s then
                theme.mark 40

            else
                L.empty

        msg =
            case mbCell of
                Just _ ->
                    ClickAt ( x, y )

                _ ->
                    JustClick
    in
    E.onMouseDown (always msg) <| L.impose dot <| theme.cell mbCell 40


blocks : Float -> Theme a
blocks base =
    { cell = block << Maybe.map (makeColor base)
    , mark = C.filled (C.uniform Color.white) << C.circle << mul 0.25
    }


hearts : Float -> Theme a
hearts base =
    { cell = heart << Maybe.map (makeColor base)
    , mark =
        \s ->
            L.stack
                [ heartShape Color.white (s * 0.4)
                , heartShape Color.black (s * 0.5)
                ]
    }


makeColor : Float -> Cell -> Color
makeColor base cell =
    let
        mk h =
            Color.hsl h 0.8 0.6
    in
    case cell of
        C1 ->
            mk base

        C2 ->
            mk (base + 0.333)

        C3 ->
            mk (base + 0.333 + 0.333)


block : Maybe Color -> Float -> Collage a
block mbColor size =
    C.filled
        (C.uniform <|
            Maybe.withDefault Color.black mbColor
        )
    <|
        C.square size


heart : Maybe Color -> Float -> Collage a
heart mbColor size =
    let
        bg =
            C.filled (C.uniform Color.black) <| C.square size
    in
    case mbColor of
        Just c ->
            L.impose (heartShape c size) bg

        Nothing ->
            bg


heartShape : Color -> Float -> Collage a
heartShape c s =
    let
        x =
            0.27 * s

        circle =
            C.filled (C.uniform c) <| C.circle x

        square =
            C.filled (C.uniform c) <| C.square (x * 2)
    in
    C.rotate (degrees 45) <|
        L.stack
            [ C.shiftY x circle
            , C.shiftX x circle
            , square
            ]


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
                Random.uniform C1 [ C2, C3 ]


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


mul : Float -> Float -> Float
mul x y =
    x * y
