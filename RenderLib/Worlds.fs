﻿namespace RenderLib

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
        light: point_light; //TODO - support multiple lights
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

    let is_shadowed (w:world) (p:tuple) =
        let v = w.light.position - p
        let distance = v.magnitude()
        let direction = v.normalize()
        let ray = {
            origin = p;
            direction = direction;
        }
        let intersections = intersect_world w ray
        match hit intersections with
        | Some i -> i.t < distance
        | None -> false

    let rec reflected_color world comps remaining =
        if remaining < 1 then
            black
        else
            let sp = shapeToProperties comps.obj
            if sp.material.reflective = 0.0 then
                black
            else
                let reflect_ray = {
                    origin = comps.over_point;
                    direction = comps.reflectv;
                }
                let c = color_at world reflect_ray (remaining - 1)
                c * sp.material.reflective

    and shade_hit world comps remaining = 
        let shadowed = is_shadowed world comps.over_point
        let sp = shapeToProperties comps.obj
        let surface = lighting sp.material comps.obj world.light comps.point comps.eyev comps.normalv shadowed
        let reflected = reflected_color world comps remaining
        surface + reflected

    and color_at world ray remaining =
        let i = intersect_world world ray
        match hit i with 
        | Some hit -> 
            let comp = prepare_computations hit ray
            shade_hit world comp remaining
        | None -> black


