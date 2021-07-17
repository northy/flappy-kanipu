module Main exposing (main)

import Playground exposing (..)
import Simplex exposing (PermutationTable)

-- MAIN

main =
    game view update init


-- RNG

--Create a permutation table, using 42 as the seed
permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42

-- Create a function for 2D fractal noise
noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 400.0, steps = 50, stepSize = 1.0, persistence = 20000.0 } permTable

getNoise : Float -> Float
getNoise x =
    noise (x * 123) (x*12)

-- MODEL

type Model 
    = Waiting
    | GameOver
        { score : Int }
    | Running
        { patra : Patra
        , score : Int
        , iterationCount : Int
        , pipes : List Pipe
        }

type alias Pipe =
    { x : Number
    , height : Number
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
        , iterationCount = 0
        , pipes = []
        }

-- UPDATE

press : Computer -> Bool
press computer =
    computer.keyboard.up || computer.mouse.down

checkPipes : Computer -> Int -> List Pipe -> List Pipe
checkPipes computer itc pipes =
    let
        l = computer.screen.left
    in
    List.filter (\x -> x.x > (l - 60)) pipes
        |> (\list ->
            list ++ (List.range 0 (5 - (List.length list))
                |> List.map
                    (\x ->
                        Pipe ((5 - toFloat x) * 750) ((getNoise (toFloat (x + itc))) * 250)
                    )
                )
        )

update : Computer -> Model -> Model
update computer model =
    case model of
        Running run ->
            let
                bot = computer.screen.bottom+120
                top = (computer.screen.top - 70)
                dt = 1.666
                vy =
                    if press computer && (run.patra.vy<2.5) then 5
                    else
                        if run.patra.y>bot then run.patra.vy - dt / 8 else 0
                y = max bot (min top run.patra.y + dt * vy)

                speed = (10 * computer.screen.width) / 1000
                pipes =  run.pipes
                    |> List.map (\pipe -> {pipe | x = pipe.x - speed})
                    |> checkPipes computer run.iterationCount
            in
            if y /= bot then
                Running
                { patra =
                    { y = y
                    , vy = vy
                    }
                , score = run.score
                , iterationCount = run.iterationCount+1
                , pipes = pipes
                }
            else
                GameOver
                { score = run.score }
        _ ->
            if press computer then initRun else model

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
                List.map (\pipe -> viewPipes computer pipe.x pipe.height) run.pipes ++
                [ image 100 100 (toGif run.patra)
                    |> move (l+60) (run.patra.y)
                , viewPoints computer run.score
                ]
        Waiting ->
            
            [ viewStatusText computer.screen.width "こんばんわんわん!" ]
        GameOver info ->
            [ viewStatusText computer.screen.width "ゲームオーバー!"
                |> moveY 20
            , viewPoints computer info.score
            ]

toGif : Patra -> String
toGif _ =
    "public/img/patra.png"

viewPipes : Computer -> Number -> Number -> Shape
viewPipes computer x y =
    let
        t = computer.screen.top
        b = computer.screen.bottom
        gate = (1000 * 35) / computer.screen.width
    in
    group
    -- top pipe
    [ image 160 800 "public/img/pipe.png"
        |> move x (y + t + gate)
        |> rotate 180
    -- bottom pipe
    , image 160 800 "public/img/pipe.png"
        |> move x (y + b - gate)
    ]

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

viewStatusText : Number -> String -> Shape
viewStatusText width status =
    group
    [ image 200 200 "public/img/kanikama.png"
        |> moveY 150
    , (words black status)
        |> scale 3
    , if width>=1000 then
        (words black "続けるには上矢印キーを押してください...")
            |> scale 3
            |> moveY -70
        else
            group
            [ (words black "続けるには上矢印キー")
                |> scale 3
                |> moveY -70
            , (words black "を押してください...")
                |> scale 3
                |> moveY -140
            ]
    ]
        |> moveY (if width>=1000 then -35 else 0)

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
