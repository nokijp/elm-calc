module CursorTests exposing (tests)

import Expect
import Test exposing (..)

import Cursor exposing (..)

tests : Test
tests =
  describe "Cursor"
    [ describe "reverse" <|
        [test "it should reverse the list" <| \_ -> reverse (Cursor [1, 0] 2 [3, 4]) |> Expect.equal (Cursor [3, 4] 2 [1, 0])]
    , describe "forward" <|
        let
          patterns =
            [ (Cursor [] 0 [1, 2, 3, 4], Just <| Cursor [0] 1 [2, 3, 4])
            , (Cursor [0] 1 [2, 3, 4], Just <| Cursor [1, 0] 2 [3, 4])
            , (Cursor [2, 1, 0] 3 [4], Just <| Cursor [3, 2, 1, 0] 4 [])
            , (Cursor [3, 2, 1, 0] 4 [], Nothing)
            ]
        in
          patterns |> List.map
            (\(c, r) ->
              test ("it should forward " ++ Debug.toString c) <|
                \_ -> forward c |> Expect.equal r
            )
    , describe "back" <|
        let
          patterns =
            [ (Cursor [] 0 [1, 2, 3, 4], Nothing)
            , (Cursor [0] 1 [2, 3, 4], Just <| Cursor [] 0 [1, 2, 3, 4])
            , (Cursor [2, 1, 0] 3 [4], Just <| Cursor [1, 0] 2 [3, 4])
            , (Cursor [3, 2, 1, 0] 4 [], Just <| Cursor [2, 1, 0] 3 [4])
            ]
        in
          patterns |> List.map
            (\(c, r) ->
              test ("it should back " ++ Debug.toString c) <|
                \_ -> back c |> Expect.equal r
            )
    , describe "update" <|
        [test "it should update the current value" <| \_ -> update 5 (Cursor [1, 0] 2 [3, 4]) |> Expect.equal (Cursor [1, 0] 5 [3, 4])]
    ]
