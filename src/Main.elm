module Main exposing (main)

import Playground exposing (..)
import Simplex exposing (PermutationTable)

-- MAIN

main =
    game view update init

-- RNG

permTable : PermutationTable
permTable =
    Simplex.permutationTableFromInt 42

noise : Float -> Float -> Float
noise =
    Simplex.fractal2d { scale = 400.0, steps = 50, stepSize = 1.0, persistence = 20000.0 } permTable

getNoise : Number -> Float
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
        , iterationCount = 1000
        , pipes = 
            [ Pipe 1050 ((getNoise (toFloat 250) * 300))
            , Pipe 1800 ((getNoise (toFloat 500) * 300))
            , Pipe 2550 ((getNoise (toFloat 750) * 300))
            , Pipe 3300 ((getNoise (toFloat 1000) * 300))
            ]
        }

-- UPDATE

press : Computer -> Bool
press computer =
    computer.keyboard.space || computer.mouse.down || computer.keyboard.up

checkPipes : Computer -> List Pipe -> List Pipe
checkPipes computer pipes =
    let
        l = computer.screen.left
    in
        List.filter (\x -> x.x > (l - 60)) pipes

addPipes : Int -> List Pipe -> List Pipe
addPipes itc pipes =
    if List.length pipes < 4 then
        pipes ++ [ Pipe 2250 ((getNoise (toFloat itc)) * 250) ]
    else pipes

checkPipeCollision : Computer -> Patra -> List Pipe -> Bool
checkPipeCollision computer patra pipes = 
    let
        l = computer.screen.left
        gate = 120
        lastpipe = List.head pipes
    in
        case lastpipe of
            Just pipe ->
                pipe.x <= l + 160 && (patra.y >= (pipe.height + gate - 50) || patra.y <= (pipe.height - gate + 50))
            _ -> False

update : Computer -> Model -> Model
update computer model =
    case model of
        Running run ->
            let
                bot = computer.screen.bottom+100
                top = (computer.screen.top - 70)
                dt = 1.666
                vy =
                    if press computer && (run.patra.vy<2.5) then 5
                    else
                        if run.patra.y>(bot) then run.patra.vy - dt / 8 else 0
                y = max (bot) (min top run.patra.y + dt * vy)

                speed = (10 * computer.screen.width) / 1000
                list = run.pipes
                    |> List.map (\pipe -> {pipe | x = pipe.x - speed})
                    |> checkPipes computer
                score =  if List.length list < 4 then run.score + 1 else run.score
                pipes = addPipes run.iterationCount list
                collision = y == (bot) || checkPipeCollision computer run.patra run.pipes
            in
            if not collision then
                Running
                { patra =
                    { y = y
                    , vy = vy
                    }
                , score = score
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
                l = computer.screen.left
            in
                List.map (\pipe -> viewPipes computer pipe.x pipe.height) run.pipes ++
                [ image 100 100 (toGif run.patra)
                    |> move (l+60) (run.patra.y)
                --, words black (String.fromInt run.iterationCount)
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
toGif patra =
    if patra.vy >= 3 then
        "public/img/patra-up.png"
    else
        "public/img/patra-down.png"

viewPipes : Computer -> Number -> Number -> Shape
viewPipes computer x y =
    let
        t = computer.screen.top
        b = computer.screen.bottom
        gate = 120 --(1600 * 60) / computer.screen.height
    in
    group
    -- top pipe
    [ image 160 800 "public/img/pipe.png"
        |> rotate 180
        |> move x (t - 80 + y + gate)
    -- bottom pipe
    , image 160 800 "public/img/pipe.png"
        |> move x (b + 80 + y - gate)
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
