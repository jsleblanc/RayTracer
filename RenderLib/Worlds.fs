namespace RenderLib

open System
open Common
open Tuple
open Color
open Matrix
open Ray
open Lights
open Shapes

module Worlds = 

    type world = {
        light: point_light;
        objs: shape list
    } with static member Default = {
            light = {
                position = point -10.0 10.0 -10.0;
                intensity = color 1.0 1.0 1.0
            }
            objs = []
    }

    let addShape world shape =
        let shapes = world.objs @ [shape]
        { world with objs = shapes }

    let intersect_world world ray =
        world.objs
        |> Seq.map (fun (shape) -> intersect shape ray)
        |> Seq.collect (fun c -> c)
        |> Seq.sortBy (fun i -> i.t)