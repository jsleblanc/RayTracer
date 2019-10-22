﻿namespace RenderLib

open System
open Common
open Color
open Tuple
open Matrix
open Ray
open Material
open Patterns

module Shapes = 

    type shapeProperties = {
        identity: Guid;
        material: material;
        default_transformation: matrix;
    } with static member Default = {
            identity = Guid.NewGuid ();
            material = material.Default;
            default_transformation = identity_matrix ();
        }

    type shape =
    | Sphere of shapeProperties
    | Plane of shapeProperties

    type intersection = {
        t: float;
        obj: shape;
    }

    type precomputed = {
        t: float;
        obj: shape;
        point: tuple;
        eyev: tuple;
        normalv: tuple;
        reflectv: tuple;
        inside: bool;
        over_point: tuple;
        under_point: tuple;
        n1: float;
        n2: float;
    }

    let shapeToProperties shape =
        match shape with
        | Sphere s -> s
        | Plane p -> p

    let private local_normal_at shape pt =
        match shape with
        | Sphere _ -> pt - (point 0.0 0.0 0.0)
        | Plane _ -> vector 0.0 1.0 0.0

    let private local_intersect shape local_ray =
        let normal = local_normal_at shape local_ray.origin
        match shape with
        | Sphere _ ->
            let a = local_ray.direction.dotProduct(local_ray.direction)
            let b = 2.0 * local_ray.direction.dotProduct(normal)
            let c = normal.dotProduct(normal) - 1.0
            let discriminant = b**2.0 - 4.0 * a * c
            if discriminant < 0.0 then
                Seq.empty<intersection>
            else 
                let t1 = (-b - Math.Sqrt(discriminant)) / (2.0 * a)
                let t2 = (-b + Math.Sqrt(discriminant)) / (2.0 * a)
                seq {
                    { t = t1; obj = shape; }; 
                    { t = t2; obj = shape; };
                }
        | Plane p -> 
            if Math.Abs(local_ray.direction.y) < epsilon then
                Seq.empty<intersection>
            else
                seq [{ t = -local_ray.origin.y / local_ray.direction.y; obj = Plane p; }]

    let intersect shape ray =
        let sp = shapeToProperties shape
        let local_ray = transform ray (inverse sp.default_transformation)
        local_intersect shape local_ray

    let hit (intersections:seq<intersection>) : intersection option = 
        let filtered = intersections |> Seq.filter (fun i -> i.t > 0.0)
        if Seq.isEmpty filtered then
            None
        else 
            let lowest = Seq.minBy (fun (i:intersection) -> i.t) filtered
            Some lowest

    let normal_at shape pt =
        let sp = shapeToProperties shape
        let i = inverse sp.default_transformation
        let local_point = i * pt
        let local_normal = local_normal_at shape local_point
        let world_normal = i.Transpose * local_normal
        let v = {
            x = world_normal.x;
            y = world_normal.y;
            z = world_normal.z;
            w = 0.0;
        }
        v.normalize()

    //not happy with this, still an "imperative" algorithm, need to figure out how to do it functionally
    let rec private func (hit:intersection) pos (xs:seq<intersection>) (container:list<shape>) n1 n2 =
        let mutable c = container
        let mutable exit = false
        let i = xs |> Seq.item pos
        let isHit = i = hit
        let n1 = 
            if isHit then
                if Seq.isEmpty container then
                    1.0
                else
                    let sp = shapeToProperties (Seq.last c)
                    sp.material.refractive_index
            else
                n1
        c <- if c |> List.contains i.obj then
                List.filter (fun (s) -> (s = i.obj) |> not) c
             else
                List.append c [i.obj]
        let n2 = 
            if isHit then
                exit <- true
                if Seq.isEmpty c then
                    1.0
                else
                    let sp = shapeToProperties (Seq.last c)
                    sp.material.refractive_index
            else
                n2
        if exit |> not && pos < (Seq.length xs) then
            func hit (pos+1) xs c n1 n2
        else
            (n1, n2)

    let prepare_computations (hit:intersection) ray (xs:seq<intersection>) = 
        let p = position ray hit.t
        let zero_point = point 0.0 0.0 0.0
        let comps = {
            t = hit.t;
            obj = hit.obj;
            point = p;
            eyev = -ray.direction;
            normalv = normal_at hit.obj p;
            reflectv = vector 0.0 0.0 0.0;
            inside = false;
            over_point = zero_point;
            under_point = zero_point;
            n1 = 0.0;
            n2 = 0.0;
        }
        let newComps =
            if comps.normalv.dotProduct comps.eyev < 0.0 then
                { comps with inside = true; normalv = -comps.normalv; }
            else
                comps
        let over_point = newComps.point + newComps.normalv * epsilon
        let under_point = newComps.point - newComps.normalv * epsilon
        let reflectv = reflect ray.direction newComps.normalv
        let (n1,n2) = func hit 0 xs List.empty<shape> 1.0 1.0
        { newComps with over_point = over_point; under_point = under_point; reflectv = reflectv; n1 = n1; n2 = n2; }
    
    let shapeWithColor shape color = 
        let sp = shapeToProperties shape
        { sp with material = { sp.material with color = color; }}

    let shapeWithTransformation shape matrix =
        let sp = shapeToProperties shape
        { sp with default_transformation = matrix; }

    let pattern_at_object (pattern:pattern) object (pt:tuple) =
        let sp = shapeToProperties object
        let object_point = inverse sp.default_transformation * pt
        pattern_at pattern object_point
