module Main exposing (main)

import Playground exposing (..)
import Html.Attributes exposing (shape)

-- MAIN

main =
    game view update init

-- MODEL

type Model 
    = Waiting
    | GameOver
        { score : Int }
    | Running
        { patra : Patra
        , score : Int
        }

type alias Patra =
    { y : Float
    , vy : Float
    }

init : Model
init = Waiting

initRun : Model
initRun =
    Running
        { patra =
            { y = 0
            , vy = 0
            }
        , score = 0
        }

-- UPDATE

update : Computer -> Model -> Model
update computer model =
    case model of
        Running run ->
            let
                bot = computer.screen.bottom+120
                top = (computer.screen.top - 70)
                dt = 1.666
                vy =
                    if computer.keyboard.up && (run.patra.vy<0) then 5
                    else
                        if run.patra.y>bot then run.patra.vy - dt / 8 else 0
                y = max bot (min top run.patra.y + dt * vy)
            in
            if y /= bot then
                Running
                { patra =
                    { y = y
                    , vy = vy
                    }
                , score = run.score
                }
            else
                GameOver
                { score = run.score }
        Waiting ->
            if computer.keyboard.up then initRun else model
        GameOver _ ->
            if computer.keyboard.up then initRun else model

-- VIEW

view : Computer -> Model -> List Shape
view computer model =
    viewBackground computer ++
    case model of
        Running run ->
            let
                --w = computer.screen.width
                --h = computer.screen.height
                --b = computer.screen.bottom
                l = computer.screen.left
            in
                [ image 140 140 (toGif run.patra)
                        |> move (l+60) (run.patra.y)
                , viewPoints computer run.score
                ]
        Waiting ->
            
            [ viewStatusText "こんばんわんわん!"
                    |> moveY 20
            ]
        GameOver info ->
            [ viewStatusText "ゲームオーバー!"
                    |> moveY 20
            , viewPoints computer info.score
            ]

toGif : Patra -> String
toGif _ =
    "public/img/patra.png"

viewBackground : Computer -> List Shape
viewBackground computer =
    let
        w = computer.screen.width
        h = computer.screen.height
        b = computer.screen.bottom
    in
        [
        rectangle (rgb 174 238 238) w h
        , rectangle (rgb 74 163 41) w 100
                |> moveY b
        ]

viewStatusText : String -> Shape
viewStatusText status =
    group
    [ (words black status)
            |> scale 3
    , (words black "続けるには上矢印キーを押してください...")
            |> scale 3
            |> moveY -70
    ]

viewPoints : Computer -> Int -> Shape
viewPoints computer points =
    let
        b = computer.screen.bottom
        r = computer.screen.right
    in
        group
        [ image 50 50 "public/img/kanikama.png"
        , words black (String.fromInt points)
            |> moveRight 50
            |> moveDown 10
        ]
            |> move (r - 150) (b + 50)
            |> scale 2
        
        
