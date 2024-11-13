module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Random
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, y)
import Time


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
    { snake = [ initialSnakePosition ]
    , direction = ( 0, 0 )
    , food = initialFoodPosition
    , growing = False
    }


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ onKeyDown (Decode.field "key" Decode.string |> Decode.andThen keyDecoder)
        , Time.every snakeSpeedInMilliseconds (\_ -> Move)
        ]


keyDecoder : String -> Decode.Decoder Msg
keyDecoder key =
    Decode.succeed (ChangeDirection key)


generateRandomPosition : Random.Generator ( Int, Int )
generateRandomPosition =
    Random.map2
        (\x y -> ( x * gridSize, y * gridSize ))
        (Random.int 0 maxGridIndex)
        (Random.int 0 maxGridIndex)


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

                    headPosition =
                        List.head newModel.snake |> Maybe.withDefault ( 0, 0 )
                in
                if headPosition == model.food then
                    ( { newModel | growing = True }
                    , Random.generate PlaceFood generateRandomPosition
                    )

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
            ( { model | food = newPosition }, Cmd.none )


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
    x < 0 || x >= boardSize || y < 0 || y >= boardSize


view : Model -> Html Msg
view model =
    div []
        [ svg
            [ width (String.fromInt boardSize)
            , height (String.fromInt boardSize)
            , style "border" (String.concat [ String.fromInt borderSize, "px solid black" ])
            ]
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


gridSize : Int
gridSize =
    20


boardSize : Int
boardSize =
    400


borderSize : Int
borderSize =
    2


maxGridIndex : Int
maxGridIndex =
    (boardSize // gridSize) - 1


initialSnakePosition : ( Int, Int )
initialSnakePosition =
    ( boardSize // 2, boardSize // 2 )


initialFoodPosition : ( Int, Int )
initialFoodPosition =
    ( (boardSize * 3) // 4, (boardSize * 3) // 4 )


snakeSpeedInMilliseconds : Float
snakeSpeedInMilliseconds =
    200
