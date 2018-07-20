module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


--

import Siteswap exposing (renderExpr)


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
    { expr : String }


model : Model
model =
    { expr = default_expression }



------------
-- UPDATE --
------------


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newExpr ->
            { model | expr = newExpr }



----------
-- VIEW --
----------


view : Model -> Html Msg
view model =
    div []
        [ div [ class "row" ]
            [ div [ class "input-field col s12" ]
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
            ]
        , br [] []
        , div [ style [ ( "text-align", "center" ) ] ]
            [ Siteswap.renderExpr
                600
                model.expr
            ]
        ]
