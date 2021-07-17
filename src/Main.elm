module Main exposing (main)

import Playground exposing (..)

-- MAIN

main =
    game view update init

-- MODEL

type alias Model =
    { patra : Patra
    }

type alias Patra =
    { y : Float
    , vy : Float
    }

init : Model
init =
    { patra =
        { y = 0
        , vy = 0
        }
    }

-- VIEW

view computer model =
    let
        w = computer.screen.width
        h = computer.screen.height
        b = computer.screen.bottom
        l = computer.screen.left
    in
    [ rectangle (rgb 174 238 238) w h
    , rectangle (rgb 74 163 41) w 100
            |> moveY b
    , image 140 140 (toGif model.patra)
            |> move (l+60) (model.patra.y)
    ]


toGif : Patra -> String
toGif _ =
    "public/img/patra.png"

-- UPDATE

update : Computer -> Model -> Model
update computer model =
    let
        bot = computer.screen.bottom+120
        top = (computer.screen.top-70)
        dt = 1.666
        vy =
            if computer.keyboard.up && (model.patra.vy<0) then 5
            else
                model.patra.vy - dt / 8
        y = model.patra.y + dt * vy
    in
    {
        patra =
            { y = max bot (min top y)
            , vy = vy
            }
    }
