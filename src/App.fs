module App

open Fable.Core
open Fable.Import
open Elmish



module Coordinate = type T = int * int

type Rotation = North | East | South | West
module Knob =
  type Color = Yellow | Red | Green | Blue | Orange
  type T = {
    color: Color;
    rotation: Rotation;
    location: Coordinate.T;
  }

  let create color rotation location = 
    {color = color; rotation = rotation; location = location}

  let rotate knob = 
    let newRotation = 
      match knob.rotation with
      | North -> East
      | East -> South
      | South -> West
      | West -> North
    {knob with rotation = newRotation}

  let location knob = knob.location

  let color knob = 
    match knob.color with
    | Yellow -> "yellow"
    | Red -> "red"
    | Green -> "green"
    | Blue -> "blue"
    | Orange -> "orange"

  let transferColor from to_ = {to_ with color = from.color}
  let sameColor k1 k2 = k1.color = k2.color

  let sameLocation k1 k2 = k1.location = k2.location
  let rotationInDegrees knob = 
    match knob.rotation with
    | North -> 0
    | East -> 90
    | South -> 180
    | West -> 270

  let transformToDirections = function
  | North -> (North, East)
  | East -> (East, South)
  | South -> (South, West)
  | West -> (West, North)

  let mapCoordinate (x, y) = function
  | North -> (x, y-1)
  | East -> (x+1, y)
  | South -> (x, y+1)
  | West -> (x-1, y)

  let oppositeRotation r1 r2 =
    match (r1, r2) with
    | (North, South) ->  true
    | (East, West) ->  true
    | (South, North) ->  true
    | (West, East) ->  true
    | _ ->  false

  let connected board knob =
    let (one, two) = transformToDirections knob.rotation in
    let coordinateOne = mapCoordinate knob.location one in
    let coordinateTwo = mapCoordinate knob.location two in
    List.filter (fun k -> 
      (
      let (oneOther, twoOther) = transformToDirections k.rotation
      (k.location = coordinateOne && (oppositeRotation one oneOther)) ||
      (k.location = coordinateOne && (oppositeRotation one twoOther)) ||
      (k.location = coordinateTwo && (oppositeRotation two oneOther)) ||
      (k.location = coordinateTwo && (oppositeRotation two twoOther)))
    ) board

type State = WaitingForInput | Resolving | Won
type Model = {
  board: Knob.T list;
  state: State
}

type Msg =
  | KnobClicked of Knob.T

let init _ = (
  {
  board = 
    [
    Knob.create Knob.Green North (0, 0);
    Knob.create Knob.Blue West (0, 1);
    Knob.create Knob.Red East (1, 0);
    Knob.create Knob.Yellow South (1, 1);
    Knob.create Knob.Red South (2, 0);
    Knob.create Knob.Orange South (2, 1);
    Knob.create Knob.Blue South (2, 2);
    Knob.create Knob.Green South (0, 2);
    Knob.create Knob.Green South (1, 2);
    ];
  state = WaitingForInput
}, Cmd.none)

let updateBoard board newKnobs =
  board
  |> List.filter (fun k1 -> 
    List.forall (fun k2 -> not (Knob.sameLocation k1 k2)) newKnobs)
  |> List.append newKnobs

let update msg model = 
  match msg with
  | KnobClicked knob -> 
    let knob = Knob.rotate knob in
    let connected = 
      Knob.connected model.board knob
      |> List.map (Knob.transferColor knob)
    in
    let newCmds = 
      List.map (KnobClicked >> Cmd.ofMsg) connected
    in
    let newKnobs = (knob :: connected) in
    let newBoard =updateBoard model.board newKnobs in
    let gameWon = 
      match model.board with
      | [] -> true
      | (hd::rest) -> List.forall (fun knob -> Knob.sameColor hd knob) rest
    in
    match (gameWon, newCmds) with
    | (true, _) -> 
      (
        {board = newBoard; state = Won}, 
        Cmd.none)
    | (false, []) -> 
      (
        {board = newBoard; state = WaitingForInput}, 
        Cmd.batch newCmds)
    | (false, _) -> 
      (
        {board = newBoard; state = Resolving}, 
        Cmd.batch newCmds)

open Elmish.React.Components
module R = Fable.Helpers.React
module A = Fable.Helpers.React.Props
let viewKnob dispatch knob = 
  let (x,y) = Knob.location knob in
  let size = 50 in
  let toPx i = (string i) + "px" in
  let translate = "translate(" + string (x * size * 2) + ", " + string (y * size * 2) + ")" in
  let rotate = 
    let degrees = Knob.rotationInDegrees knob in
    "rotate(" + string (degrees) + ", " + string (x * size * 2 + size) + ", " + string (y * size * 2 + size) + ")" 
  in
  let transform = rotate + " " + translate in
  R.g [A.OnClick (fun _ -> KnobClicked knob |> dispatch); A.Transform transform] [
    R.circle [A.Cx (toPx size); A.Cy (toPx size); A.R (toPx size); A.Fill (Knob.color knob)] [];
    R.path [A.D "M 50 0 V 50 H 100"; A.Stroke "black"; A.StrokeWidth "5"; A.Fill "transparent"] []
  ]

let viewBoard board dispatch =
  R.svg [A.Width "500px"; A.Height "500px"] (
    List.map (viewKnob dispatch) board
  )

let view model dispatch =
  let state = 
    match model.state with
    | WaitingForInput -> "waiting for input"
    | Won -> "won"
    | Resolving -> "resolving"
  in
  R.div
    []
    [
      R.str state;
      viewBoard model.board dispatch
    ]


open Elmish.React

// App
Program.mkProgram init update view 
|> Program.withConsoleTrace
|> Program.withReact "elmish-app"
|> Program.run