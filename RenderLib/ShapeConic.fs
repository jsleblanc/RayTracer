namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes2

module ShapeConic =

    let parameters_of shape =
        match shape with
        | Cylinder (minimum,maximum,closed) -> (minimum,maximum,closed)
        | Cone (minimum,maximum,closed) -> (minimum,maximum,closed)
        | _ -> raise (Exception("expected a cylinder or a cone"))

    (*
    module Cylinder =
    
    let build minimum maximum closed = 
        let local_normal_at shape pt = 
            let dist = pt.x**2.0 + pt.z**2.0
            if dist < 1.0 && pt.y >= (max - epsilon) then
                vector 0.0 1.0 0.0
            else 
                if dist < 1.0 && pt.y <= (min + epsilon) then
                    vector 0.0 -1.0 0.0
                else
                    vector pt.x 0.0 pt.z
        let local_intersect shape ray =
            let check_cap t =
                let x = ray.origin.x + t * ray.direction.x
                let z = ray.origin.z + t * ray.direction.z
                (x**2.0 + z**2.0) <= 1.0
            let intersect_caps =
                if not closed || (areEqualFloat ray.direction.y 0.0) then
                    Seq.empty<intersection>
                else
                    let calc v =
                        let t = (v - ray.origin.y) / ray.direction.y
                        if check_cap t then
                            Some { t = t; obj = shape;}
                        else
                            None
                    seq { calc cmin; calc cmax; } |> Seq.choose id
            let a = ray.direction.x**2.0 + ray.direction.z**2.0
            if areEqualFloat a 0.0 then
                intersect_caps
            else
                let b = 2.0 * ray.origin.x * ray.direction.x + 2.0 * ray.origin.z * ray.direction.z
                let c = ray.origin.x**2.0 + ray.origin.z**2.0 - 1.0
                let disc = b**2.0 - 4.0 * a * c
                if disc < 0.0 then
                    Seq.empty<intersection>
                else
                    let (t0,t1) = swapIfGreater ((-b - Math.Sqrt(disc)) / (2.0 * a)) ((-b + Math.Sqrt(disc)) / (2.0 * a))
                    let calc t =
                        let y = ray.origin.y + t * ray.direction.y
                        if cmin < y && y < cmax then
                            Some { t = t; obj = shape; }
                        else
                            None
                    let s1 = seq { calc t0; calc t1; } |> Seq.choose id 
                    let s2 = intersect_caps
                    s1 |> Seq.append s2
        let bounds_of shape = { minimum = point -1.0 min -1.0; maximum = point 1.0 max 1.0; }
        build (Cylinder(minimum,maximum,closed)) local_intersect local_normal_at bounds_of
    *)  