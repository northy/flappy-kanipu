module Main exposing (main)

import Playground exposing (..)
import Simplex exposing (PermutationTable)
import Platform.Cmd exposing (none)

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

getNoise : Float -> Float
getNoise x =
    noise (x * 123) (x*12)

-- MODEL

type Model 
    = Waiting
    | Loading
        { iterationCount : Int }
    | Running
        { patra : Patra
        , score : Int
        , iterationCount : Int
        , pipes : List Pipe
        , alive : Bool
        }

type alias Pipe =
    { x : Float
    , height : Float
    }

type alias Patra =
    { y : Float
    , vy : Float
    }

init : Model
init = Loading { iterationCount = 0 }

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
        , alive = True
        }

-- UPDATE

originalX : Float
originalX = 1536

propX : Computer -> Float
propX computer = computer.screen.width/originalX

scaleX : Computer -> Float -> Float
scaleX computer x =
    x * propX computer

---

originalY : Float
originalY = 758

propY : Computer -> Float
propY computer = computer.screen.height/originalY

scaleY : Computer -> Float -> Float
scaleY computer y =
    y * propY computer

press : Computer -> Bool
press computer =
    computer.keyboard.space || computer.mouse.down || computer.keyboard.up

checkPipes : Computer -> List Pipe -> List Pipe
checkPipes computer pipes =
    let
        l = computer.screen.left
    in
        List.filter (\x -> x.x > (l - (scaleX computer 100))) pipes

addPipes : Int -> List Pipe -> List Pipe
addPipes itc pipes =
    if List.length pipes < 4 then
        pipes ++ [ Pipe 2250 ((getNoise (toFloat itc)) * 250) ]
    else pipes

checkPipeCollision : Computer -> Patra -> List Pipe -> Bool
checkPipeCollision computer patra pipes = 
    let
        l = computer.screen.left
        gate = (scaleY computer 120)
        lastpipe = List.head pipes
    in
        case lastpipe of
            Just pipe ->
                pipe.x <= l + (scaleY computer 160) && (
                patra.y + (scaleY computer 50) >= (scaleX computer pipe.height) + gate ||
                patra.y - (scaleY computer 50) <= (scaleX computer pipe.height) - gate)
            _ -> False

update : Computer -> Model -> Model
update computer model =
    case model of
        Running run ->
            if computer.screen.width >= computer.screen.height then
                let
                    bot = computer.screen.bottom + (scaleY computer 50)
                    top = (computer.screen.top - 70)
                    dt = (scaleY computer 1.666)
                    pressed = press computer
                    vy =
                        if pressed && (run.patra.vy < (scaleY computer 2.5)) then (scaleY computer 5)
                        else
                            if run.patra.y>bot then run.patra.vy - dt / 8 else 0
                    y = min top run.patra.y + dt * vy

                    speed = (10 * computer.screen.width) / 1000
                    list = run.pipes
                        |> List.map (\pipe -> {pipe | x = pipe.x - speed})
                        |> checkPipes computer
                    score =  if List.length list < 4 then run.score + 1 else run.score
                    pipes = addPipes run.iterationCount list
                    collision = y - (scaleY computer 50) < bot || checkPipeCollision computer run.patra run.pipes
                    alive = run.alive && not collision
                in
                if alive then
                    Running
                    { patra =
                        { y = y
                        , vy = vy
                        }
                    , score = score
                    , iterationCount = run.iterationCount+1
                    , pipes = pipes
                    , alive = alive
                    }
                else
                    if pressed then initRun else Running {run | alive = False}
            else model
        Loading load ->
            if load.iterationCount == 50 then Waiting else Loading {load | iterationCount=load.iterationCount+1}
        _ ->
            if press computer then initRun else model

-- VIEW

view : Computer -> Model -> List Shape
view computer model =
    viewBackground computer ++
    if computer.screen.width >= computer.screen.height then
        
        case model of
            Running run ->
                let
                    l = computer.screen.left
                in
                    List.map (\pipe -> viewPipes computer pipe.x pipe.height) run.pipes ++
                    [ image (scaleX computer 100) (scaleY computer 100) (toGif computer run.patra)
                        |> move (l + (scaleX computer 60)) (run.patra.y)
                    , viewPoints computer run.score
                    ]
                    ++ if not run.alive then
                        [viewStatusText computer "ゲームオーバー!"
                            |> moveY 20
                        ]
                    else []
            Loading _ ->
                [ words black "読み込み中"
                    |> scale (scaleY computer 3)
                , group 
                    [image 1 1 "public/img/patra-up.png"
                    , image 1 1 "public/img/patra-down.png"
                    , image 1 1 "public/img/pipe.png"
                    , image 1 1 "public/img/kanipu.png"
                    , image 1 1 "public/img/kanikama.png"
                    ]
                    |> moveY -500
                ]
            Waiting ->
                [ viewStatusText computer "こんばんわんわん!" ]
    else
        [ image (scaleX computer 200) (scaleY computer 200) "public/img/rotate.png"
            |> moveY (scaleY computer 150)
        , words black "画面を回して"
            |> scale (scaleY computer 3)
        ]

toGif : Computer -> Patra -> String
toGif computer patra =
    if patra.vy >= (scaleY computer 3) then
        "public/img/patra-up.png"
    else
        "public/img/patra-down.png"

viewPipes : Computer -> Float -> Float -> Shape
viewPipes computer x y =
    let
        gate = (scaleY computer 120)
    in
    group
    -- top pipe
    [ image (scaleX computer 200) (scaleY computer 2000) "public/img/pipe.png"
        |> rotate 180
        |> move x ((scaleY computer y) + (scaleY computer 1000) * 0.8 + gate)
        |> scale 0.8
    -- bottom pipe
    , image (scaleX computer 200) (scaleY computer 2000) "public/img/pipe.png"
        |> move x ((scaleY computer y) - (scaleY computer 1000) * 0.8 - gate)
        |> scale 0.8
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
        , rectangle (rgb 74 163 41) w (scaleY computer 100)
            |> moveY b
        ]

viewStatusText : Computer -> String -> Shape
viewStatusText computer status =
    group
    [ image (scaleX computer 200) (scaleY computer 200) "public/img/kanikama.png"
        |> moveY (scaleY computer 150)
    , words black status
        |> scale (scaleY computer 3)
    , words black "続けるには上矢印キーを押してください..."
        |> scale (scaleY computer 3)
        |> moveY (scaleY computer -70)
    ]

viewPoints : Computer -> Int -> Shape
viewPoints computer points =
    let
        b = computer.screen.bottom
        r = computer.screen.right
    in
        group
        [ image (scaleX computer 50) (scaleY computer 50) "public/img/kanikama.png"
        , words black (String.fromInt points)
            |> moveRight (scaleX computer 50)
            |> moveDown (scaleY computer 10)
        ]
        -- todo: There may be a better way of doing this, it still isn't perfect
            |> move (r - (scaleX computer 150)) (b + (scaleY computer 50))
            |> scale (scaleY computer 2)
