module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Random
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, y)


type alias Model =
    { snake : List ( Int, Int )
    , direction : ( Int, Int )
    , food : ( Int, Int )
    , growing : Bool
    }


type Msg
    = Move
    | ChangeDirection String
    | PlaceFood ( Int, Int )


main =
    Browser.element
        { init = \() -> ( initModel, Cmd.none )
        , update = update
        , subscriptions = always subscriptions
        , view = view
        }


initModel : Model
initModel =
    { snake = [ ( 200, 200 ) ]
    , direction = ( 0, 0 )
    , food = ( 300, 300 )
    , growing = False
    }


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ onKeyDown (Decode.field "key" Decode.string |> Decode.andThen keyDecoder), Browser.Events.onAnimationFrameDelta (\_ -> Move) ]


keyDecoder : String -> Decode.Decoder Msg
keyDecoder key =
    Decode.succeed (ChangeDirection key)


gridSize : Int
gridSize =
    20


generateRandomPosition : Random.Generator ( Int, Int )
generateRandomPosition =
    Random.map2
        (\x y -> ( x, y ))
        (Random.int 0 ((400 // gridSize) * gridSize - gridSize))
        (Random.int 0 ((400 // gridSize) * gridSize - gridSize))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move ->
            if detectCollision (List.head model.snake |> Maybe.withDefault ( 0, 0 )) then
                ( initModel, Cmd.none )

            else
                let
                    newModel =
                        moveSnake model
                in
                if model.growing then
                    ( newModel, Random.generate PlaceFood generateRandomPosition )

                else
                    ( newModel, Cmd.none )

        ChangeDirection keyPressed ->
            case keyPressed of
                "ArrowUp" ->
                    ( { model
                        | direction =
                            if snd model.direction /= gridSize then
                                ( 0, -gridSize )

                            else
                                model.direction
                      }
                    , Cmd.none
                    )

                "ArrowDown" ->
                    ( { model
                        | direction =
                            if snd model.direction /= -gridSize then
                                ( 0, gridSize )

                            else
                                model.direction
                      }
                    , Cmd.none
                    )

                "ArrowLeft" ->
                    ( { model
                        | direction =
                            if fst model.direction /= gridSize then
                                ( -gridSize, 0 )

                            else
                                model.direction
                      }
                    , Cmd.none
                    )

                "ArrowRight" ->
                    ( { model
                        | direction =
                            if fst model.direction /= -gridSize then
                                ( gridSize, 0 )

                            else
                                model.direction
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        PlaceFood newPosition ->
            ( { model | food = newPosition, growing = True }, Cmd.none )


moveSnake : Model -> Model
moveSnake model =
    let
        newHead =
            ( fst (List.head model.snake |> Maybe.withDefault ( 0, 0 )) + fst model.direction
            , snd (List.head model.snake |> Maybe.withDefault ( 0, 0 )) + snd model.direction
            )

        newSnake =
            if model.growing then
                newHead :: model.snake

            else
                newHead :: List.take (List.length model.snake - 1) model.snake
    in
    { model | snake = newSnake, growing = False }


detectCollision : ( Int, Int ) -> Bool
detectCollision ( x, y ) =
    x < 0 || x >= 400 || y < 0 || y >= 400


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width "400", height "400", style "border" "2px solid black" ]
            (List.map snakePart model.snake ++ [ foodPart model.food ])
        ]


snakePart : ( Int, Int ) -> Svg msg
snakePart ( xCoord, yCoord ) =
    rect
        [ x (String.fromInt xCoord)
        , y (String.fromInt yCoord)
        , width (String.fromInt gridSize)
        , height (String.fromInt gridSize)
        , fill "green"
        ]
        []


foodPart : ( Int, Int ) -> Svg msg
foodPart ( xCoord, yCoord ) =
    rect
        [ x (String.fromInt xCoord)
        , y (String.fromInt yCoord)
        , width (String.fromInt gridSize)
        , height (String.fromInt gridSize)
        , fill "red"
        ]
        []


fst : ( a, b ) -> a
fst ( a, _ ) =
    a


snd : ( a, b ) -> b
snd ( _, b ) =
    b
