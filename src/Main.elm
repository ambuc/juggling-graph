module Main exposing (..)

import Cmd.Extra as CE
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onMouseUp, onClick)
import List.Extra as LE
import Random as R
import Random.Extra as RE
import Ternary exposing ((?))
import Unwrap as U


--

import Siteswap exposing (renderExpr)
import Lib exposing (View(Linear, Circular))
import Examples exposing (examples)


-----------
-- TYPES --
-----------


type alias Model =
    { expr : String
    , view : View
    }


type Msg
    = Change String
    | ToggleView
    | Roll
    | UpdateExamplesIdx Int



-------------
-- PROGRAM --
-------------


init : ( Model, Cmd Msg )
init =
    ( { expr = ""
      , view = Circular
      }
    , CE.perform Roll
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newExpr ->
            ( { model | expr = newExpr }
            , Cmd.none
            )

        ToggleView ->
            ( { model | view = (model.view == Linear) ? Circular <| Linear }
            , Cmd.none
            )

        Roll ->
            ( model
            , R.generate UpdateExamplesIdx <| R.int 0 (List.length examples - 1)
            )

        UpdateExamplesIdx idx ->
            let
                newExpr =
                    U.maybe <| LE.getAt idx examples
            in
                if newExpr == model.expr then
                    ( model, CE.perform Roll )
                else
                    ( { model | expr = newExpr }
                    , Cmd.none
                    )



----------
-- VIEW --
----------


view : Model -> Html Msg
view model =
    let
        input_span =
            span [ class "input-field" ]
                [ Html.form []
                    [ label [ for "expr" ] [ text "Enter a siteswap" ]
                    , br [] []
                    , input
                        [ class "characterCounter"
                        , attribute "type" "text"
                        , attribute "data-length" "30"
                        , maxlength 30
                        , value model.expr
                        , onInput Change
                        ]
                        []
                    ]
                ]

        switch_span =
            span [ class "switch" ]
                [ label [ onMouseUp ToggleView ]
                    [ text "Circular"
                    , input [ attribute "type" "checkbox" ] []
                    , span [ class "lever" ] []
                    , text "Linear"
                    ]
                ]

        shuffle_span =
            span []
                [ a
                    [ class "waves-effect  btn-small red lighten-2"
                    , onClick Roll
                    ]
                    [ i [ class "material-icons left" ] [ text "shuffle" ]
                    , text "Random"
                    ]
                ]

        output_span =
            span [ style [ ( "text-align", "center" ) ] ]
                [ Siteswap.renderExpr 600 600 model.expr model.view ]
    in
        div [ class "row" ]
            [ br [] []
            , div [ class "col s12 m10 offset-m1 l8 offset-l2" ]
                [ div [ class "col s12 m6" ] [ input_span ]
                , div [ class "col s12 m6" ]
                    [ switch_span
                    , br [] []
                    , br [] []
                    , shuffle_span
                    ]
                ]
            , div [ class "col s12" ] [ output_span ]
            ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , update = update
        , init = init
        , subscriptions = always Sub.none
        }



--
