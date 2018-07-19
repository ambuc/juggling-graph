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
    -- "([44x],[44x])"
    --"1[23]4[56]"
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
    div [ style [ ( "text-align", "center" ) ] ]
        [ h1 [] [ text "juggling-graph" ]
        , Html.form []
            [ label [ for "expr" ] [ text "Enter a siteswap" ]
            , br [] []
            , input
                [ id "expr"
                , placeholder default_expression
                , onInput Change
                , maxlength 30
                ]
                []
            ]
        , br [] []
        , Siteswap.renderExpr 600 model.expr
        ]
