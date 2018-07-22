module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onMouseUp)
import Ternary exposing ((?))


--

import Siteswap exposing (renderExpr)
import Lib exposing (View(Linear, Circular))


---------------------
-- MAGIC CONSTANTS --
---------------------


default_expression : String
default_expression =
    --"([44x],[44x])"
    --"2[23]4[56]"
    "(6x,4)([42],2x)"


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-----------
-- MODEL --
-----------


type alias Model =
    { expr : String
    , view : View
    }


model : Model
model =
    { expr = default_expression
    , view = Circular
    }



------------
-- UPDATE --
------------


type Msg
    = Change String
    | ToggleView


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newExpr ->
            { model | expr = newExpr }

        ToggleView ->
            { model | view = (model.view == Linear) ? Circular <| Linear }



----------
-- VIEW --
----------


view : Model -> Html Msg
view model =
    div []
        [ -- Input
          div [ class "row" ]
            [ div [ class "input-field col s12 l6" ]
                [ Html.form []
                    [ label [ for "expr" ] [ text "Enter a siteswap" ]
                    , br [] []
                    , input
                        [ class "characterCounter"
                        , attribute "type" "text"
                        , attribute "data-length" "30"
                        , maxlength 30
                        , defaultValue default_expression
                        , onInput Change
                        ]
                        []
                    ]
                ]
            , div [ class "switch col s12 l6" ]
                [ label [ onMouseUp ToggleView ]
                    [ text "Circular"
                    , input [ attribute "type" "checkbox" ] []
                    , span [ class "lever" ] []
                    , text "Linear"
                    ]
                ]
            ]

        -- Output
        , div [ style [ ( "text-align", "center" ) ] ]
            [ Siteswap.renderExpr 500 500 model.expr model.view ]
        ]



--
