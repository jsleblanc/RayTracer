namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Lights

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
        inside: bool;
        over_point: tuple;
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

    let prepare_computations (i:intersection) ray = 
        let p = position ray i.t
        let comps = {
            t = i.t;
            obj = i.obj;
            point = p;
            eyev = -ray.direction;
            normalv = normal_at i.obj p;
            inside = false;
            over_point = point 0.0 0.0 0.0;
        }
        let newComps =
            if comps.normalv.dotProduct comps.eyev < 0.0 then
                { comps with inside = true; normalv = -comps.normalv; }
            else
                comps
        { newComps with over_point = newComps.point + newComps.normalv * epsilon;}
       
    let shapeWithColor shape color = 
        let sp = shapeToProperties shape
        { sp with material = { sp.material with color = color; }}

    let shapeWithTransformation shape matrix =
        let sp = shapeToProperties shape
        { sp with default_transformation = matrix; }