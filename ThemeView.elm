module ThemeView exposing (main)

import Browser
import Collage as C
import Collage.Render exposing (svgBox)
import Main
import Set exposing (Set)


main : Program () () Main.Msg
main =
    Browser.sandbox
        { init = ()
        , view =
            always <|
                svgBox ( 400, 400 ) <|
                    C.scale 10 <|
                    Main.viewCell
                        (Main.hearts 0.2)
                        (Set.fromList [ ( 0, 0 ) ])
                        0
                        0
                        (Just Main.C1)
        , update = always identity
        }
