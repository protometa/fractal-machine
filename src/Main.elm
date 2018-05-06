port module Main exposing (..)

import Html exposing (Html, program, input)
import Html.Events exposing (..)
import Html.Attributes exposing (type_)
import Time exposing (Time, second, millisecond)
import Array exposing (Array, fromList, length, get)
import Maybe exposing (withDefault)
import Element exposing (..)
import Element.Input exposing (checkbox)
import Element.Attributes exposing (..)
import Style exposing (..)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Measure = Array Float

type alias Model =
  { on : Bool
  , root : Float
  , tick : Int
  , gain : Float
  , frequency : Float
  , iterations : List Int
  , measure : Measure
  }


init : (Model, Cmd Msg)
init =
  let
    model = 
      { on = True
      , root = 600
      , tick = 0
      , gain = 0.8
      , frequency = 0
      , iterations = [0, 2, 3]
      , measure = fromList [1, (2/3), (3/4), (4/5)]
      }
  in
    ( model, sendAudio model )


-- UPDATE

type Msg
  = Tick Time
  | Toggle Bool


type alias Root = Float

getWithDefault1 arr i = arr |> get i |> withDefault 1

--beat : Int -> Int -> Int

factor tick measure itr =
  getWithDefault1 measure <| beat tick (length measure) itr

beat tick len itr =
  tick // (len ^ itr) % len

note tick measure iterations =
  iterations |> List.map (factor tick measure) |> List.foldr (*) 1

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Tick newTime ->
      let
        tick = model.tick + 1
        newModel =
          { model | tick = tick
          , frequency = model.root * (note tick model.measure model.iterations)
          }
      in
        ( newModel, sendAudio model )

    Toggle state ->
      let
        newModel =
          { model | on = state }
      in
        (newModel, sendAudio model )



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  if model.on then
    Time.every (200 * millisecond) Tick
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
--    , el [] (text <| toString (model.tick % (length measure)))
--    , el [] (text <| toString ((model.tick // (length measure)) % (length measure)))
--    , el [] (text <| toString model.frequency)
    --]

view model =
  layout stylesheet <|
    column None [padding 10, spacing 7]
      [ checkbox None []
        { onChange = Toggle
        , checked = model.on
        , label = el None [] empty
        , options = []
        }
      , el None [] (html <| input [ type_ "range" ] [])
      , el None [] (text <| toString model.frequency)
      ]


sendAudio : Model -> Cmd msg
sendAudio model =
    audio { frequency = model.frequency, gain = model.gain, on = model.on }

port audio : { frequency : Float, gain : Float, on : Bool } -> Cmd msg
port toggle : Bool -> Cmd msg




