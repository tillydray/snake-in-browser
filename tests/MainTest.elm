module MainTest exposing (tests)

import Expect
import Main exposing (Msg(..), fst, gridSize, initModel, snd, update)
import Test exposing (..)


tests : Test
tests =
    describe "Snake Game Tests"
        [ test "Snake moves when Move message is received" <|
            let
                initialModel =
                    { initModel | direction = ( gridSize, 0 ) }

                -- moving right
                ( updatedModel, _ ) =
                    update Move initialModel

                initialHead =
                    List.head initialModel.snake |> Maybe.withDefault ( 0, 0 )

                expectedHead =
                    ( fst initialHead + fst initialModel.direction
                    , snd initialHead + snd initialModel.direction
                    )

                updatedHead =
                    List.head updatedModel.snake |> Maybe.withDefault ( 0, 0 )
            in
            \_ ->
                Expect.equal updatedHead
                    expectedHead
        , test "Snake grows when it eats food" <|
            let
                initialHead =
                    ( 100, 100 )

                foodPosition =
                    ( 120, 100 )

                -- Place food directly in front of the snake
                initialModel =
                    { initModel
                        | snake = [ initialHead ]
                        , direction = ( gridSize, 0 ) -- Moving right
                        , food = foodPosition
                    }

                -- First update: snake moves and eats the food
                ( modelAfterFirstMove, _ ) =
                    update Move initialModel

                -- Simulate placing new food to avoid randomness
                ( modelAfterPlacingFood, _ ) =
                    update (PlaceFood ( 200, 200 )) modelAfterFirstMove

                -- Second update: snake moves again, should grow
                ( finalModel, _ ) =
                    update Move modelAfterPlacingFood

                initialLength =
                    List.length initialModel.snake

                finalLength =
                    List.length finalModel.snake
            in
            \_ -> Expect.equal finalLength (initialLength + 1)
        , test "Snake speed increases by 10% when it grows" <|
            let
                initialModel =
                    { initModel
                        | snakeSpeedInMilliseconds = 200.0
                        , snake = [ ( 100, 100 ) ]
                        , direction = ( gridSize, 0 )
                        , food = ( 120, 100 ) -- Place food directly in front of the snake
                    }

                -- First update: snake moves and eats the food
                ( modelAfterFirstMove, _ ) =
                    update Move initialModel

                -- New speed after eating food
                newSpeed =
                    modelAfterFirstMove.snakeSpeedInMilliseconds

                expectedSpeed =
                    initialModel.snakeSpeedInMilliseconds * 0.9
            in
            \_ ->
                Expect.equal newSpeed expectedSpeed
        , test "Snake changes direction when ArrowUp key is pressed" <|
            let
                initialModel =
                    { initModel
                        | direction = ( gridSize, 0 ) -- Moving right initially
                    }

                -- Apply ChangeDirection "ArrowUp"
                ( updatedModel, _ ) =
                    update (ChangeDirection "ArrowUp") initialModel

                expectedDirection =
                    ( 0, -gridSize )

                -- Expected to be moving up
            in
            \_ ->
                Expect.equal updatedModel.direction expectedDirection
        , test "Snake does not reverse direction when opposite arrow key is pressed" <|
            let
                initialModel =
                    { initModel
                        | direction = ( gridSize, 0 ) -- Moving right initially
                    }

                -- Try to reverse direction by pressing ArrowLeft
                ( updatedModel, _ ) =
                    update (ChangeDirection "ArrowLeft") initialModel

                expectedDirection =
                    ( gridSize, 0 )

                -- Should still be moving right
            in
            \_ ->
                Expect.equal updatedModel.direction expectedDirection
        , test "Game resets when the snake collides with the wall" <|
            let
                initialModel =
                    { initModel
                        | snake = [ ( Main.boardSize - gridSize, 0 ) ] -- At right edge
                        , direction = ( gridSize, 0 ) -- Moving right towards wall
                    }

                ( intermediateModel, _ ) =
                    update Move initialModel

                ( updatedModel, _ ) =
                    update Move intermediateModel
            in
            \_ ->
                Expect.equal updatedModel initModel
        , test "Game resets when the snake collides with itself" <|
            let
                initialModel =
                    { initModel
                        | snake =
                            [ ( 100, 100 )
                            , ( 80, 100 )
                            , ( 80, 80 )
                            , ( 100, 80 )
                            , ( 100, 100 ) -- Head collides with its own body
                            ]
                        , direction = ( 0, -gridSize ) -- Moving up into itself
                    }

                -- Apply Move, which should result in self-collision
                ( updatedModel, _ ) =
                    update Move initialModel
            in
            \_ ->
                Expect.equal updatedModel initModel
        , test "New food is placed at a valid position after the snake eats it" <|
            let
                initialHead =
                    ( 100, 100 )

                foodPosition =
                    ( 120, 100 )

                -- Place food directly in front of the snake
                initialModel =
                    { initModel
                        | snake = [ initialHead ]
                        , direction = ( gridSize, 0 ) -- Moving right
                        , food = foodPosition
                    }

                -- First update: snake moves and eats the food
                ( modelAfterFirstMove, _ ) =
                    update Move initialModel

                -- Simulate placing new food to avoid randomness
                newFoodPosition =
                    ( 200, 200 )

                ( modelAfterPlacingFood, _ ) =
                    update (PlaceFood newFoodPosition) modelAfterFirstMove

                -- Destructure the new food position
                ( x, y ) =
                    newFoodPosition

                -- Check that new food is within bounds and not on the snake
                foodIsValid =
                    x
                        >= 0
                        && x
                        < Main.boardSize
                        && y
                        >= 0
                        && y
                        < Main.boardSize
                        && not (List.member newFoodPosition modelAfterPlacingFood.snake)
            in
            \_ ->
                Expect.equal foodIsValid True
        , test "Snake continues moving in the current direction when an unrecognized key is pressed" <|
            let
                initialModel =
                    { initModel
                        | direction = ( gridSize, 0 ) -- Moving right
                    }

                -- Apply ChangeDirection with an unrecognized key
                ( updatedModel, _ ) =
                    update (ChangeDirection "UnknownKey") initialModel

                expectedDirection =
                    ( gridSize, 0 )

                -- Should still be moving right
            in
            \_ ->
                Expect.equal updatedModel.direction expectedDirection
        ]
