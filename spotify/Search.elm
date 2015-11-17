module Search where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Events exposing (onChange, onEnter)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Signal exposing (message,forwardTo,Address)
import List


-- MODEL


type alias Model =
    { query : String
    , answers : List Answer
    }

type alias Answer =
    { name : String
    , images : List Image
    }

type alias Image =
  { height : Int
  , width : Int
  , url : String
  }

init : (Model, Effects Action)
init =
  ( Model "" []
  , Effects.none
  )



-- UPDATE


type Action
    = QueryChange String
    | Query
    | RegisterAnswers (Maybe (List Answer))


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    QueryChange newQuery ->
      ( Model newQuery model.answers
      , Effects.none
      )

    Query ->
      ( model
      , search model.query
      )

    RegisterAnswers maybeAnswers ->
      ( Model model.query (Maybe.withDefault [] maybeAnswers)
      , Effects.none
      )



-- VIEW


containerFluid =
  div [class "container-fluid"]


row =
  div [class "row"]


bootstrap =
  node "link"
    [ href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"
    , rel "stylesheet"
    ]
    []


view : Signal.Address Action -> Model -> Html
view address model =
  div
    [style [("margin", "20px 0")]]
    [ bootstrap
    , containerFluid
        [ inputForm address model
        , resultsList address model
        ]
    ]


inputForm address model =
  input
    [ type' "text"
    , placeholder "Search for an album..."
    , value model.query
    , onChange address QueryChange
    , onEnter address Query
    ]
    []


resultsList address model =
  let
    toEntry answer =
      div
        [class "col-xs-2 col-md-3"]
        [resultView answer]
  in
    row (List.map toEntry model.answers)

resultView : Answer -> Html
resultView answer =
  let
    imgmaybe = List.head answer.images
    imgsrc = Maybe.withDefault ""  (Maybe.map .url imgmaybe)
  in
    div [class "panel panel-info"]
          [ div
            [class "panel-heading"]
            [text "Album"]
          , div
            [ class "panel-body"
            -- , style [("height", "10rem")]
            ]
            [h3 []  [text answer.name]
            , img [src imgsrc, style [("width", "100%")]] []]
      ]



-- EFFECTS


(=>) = (,)


search : String -> Effects Action
search query =
  Http.get decodeAnswers (searchUrl query)
    |> Task.toMaybe
    |> Task.map RegisterAnswers
    |> Effects.task


searchUrl : String -> String
searchUrl query =
  Http.url "https://api.spotify.com/v1/search"
    [ "q" => query
    , "type" => "album"
    ]


decodeAnswers : Json.Decoder (List Answer)
decodeAnswers =
  let
    albumImage = Json.object3 Image
                 ("height" := Json.int)
                 ("width" := Json.int)
                 ("url" := Json.string)
    album = Json.object2 Answer
            ("name" := Json.string)
            ("images" := Json.list albumImage)

  in
    (Json.at ["albums", "items"] (Json.list album))
