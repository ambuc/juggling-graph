module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


--

import Siteswap


-- MAGIC CONSTANTS


default_expression : String
default_expression =
    "3[12][22]"


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { expr : String }


model : Model
model =
    { expr = default_expression }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newExpr ->
            { model | expr = newExpr }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "juggling-graph" ]
        , input
            [ placeholder default_expression
            , onInput Change
            ]
            []
        , br [] []
        , Siteswap.renderExpr 600 model.expr
        ]
