port module Main exposing (..)

import Html exposing (Html, program, input)
import Html.Events exposing (..)
import Html.Attributes exposing (type_)
import Time exposing (Time, second, millisecond)
import Array exposing (Array, fromList, length, get)
import Maybe exposing (withDefault)
import Element exposing (..)
import Element.Input exposing (checkbox)
import Style exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
  { on : Bool
  , root : Float
  , beat : Int
  , gain : Float
  , frequency : Float
  }


init : (Model, Cmd Msg)
init =
  let
    model = 
      { on = True
      , root = 400
      , beat = 0
      , gain = 0.8
      , frequency = root
      }
  in
    ( model, sendAudio model )


-- UPDATE

type Msg
  = Tick Time
  | Toggle Bool


type alias Measure = Array Float
measure : Measure
measure = fromList [1, (2/3), (3/4), (4/5)]

type alias Root = Float
root = 350

factor beat = 
  (withDefault 1 (get (beat % (length measure)) measure))
    * (withDefault 1 (get ((beat // (length measure)) % (length measure)) measure))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Tick newTime ->
      let
        beat = model.beat + 1
        newModel =
          { model | gain = 0.8
          , beat = beat
          , frequency = root * (factor beat)
          }
      in
        ( newModel, sendAudio newModel )

    Toggle state ->
      let
        newModel =
          { model | on = state }
      in
        (newModel, sendAudio newModel)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.on then
    Time.every (750 * millisecond) Tick
  else
    Sub.none


-- VIEW

type Styles
    = None

stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style None [] ]

--view : Model -> Html Msg
--view model =
--  row None []
--    [ button [ onClick Toggle ] [text <| toString model.on]
--    , input [ type_ "range" ] []
--    , el [] (text <| toString (model.beat % (length measure)))
--    , el [] (text <| toString ((model.beat // (length measure)) % (length measure)))
--    , el [] (text <| toString model.frequency)
    --]

view model =
  Element.layout stylesheet <|
    column None []
      [ checkbox None []
        { onChange = Toggle
        , checked = model.on
        , label = el None [] (text "Playing")
        , options = []
        }
      , el None [] (html <| input [ type_ "range" ] [])
      , el None [] (text <| toString (model.beat % (length measure)))
      , el None [] (text <| toString ((model.beat // (length measure)) % (length measure)))
      , el None [] (text <| toString model.frequency)
      ]


sendAudio : Model -> Cmd msg
sendAudio model =
    audio { frequency = model.frequency, gain = model.gain, on = model.on }

port audio : { frequency : Float, gain : Float, on : Bool} -> Cmd msg
port toggle : Bool -> Cmd msg




