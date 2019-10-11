namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray

module Shapes = 

    type sphere(id : Guid) = 
        let mutable tm = identity_matrix ()
        member this.id = id
        member this.default_transformation
            with get() = tm
            and set value = tm <- value
        new() = sphere(Guid.NewGuid())

    type shape =
    | Sphere of sphere

    type intersection = {
        t: float;
        obj: shape;
    }

    let ray_from_inverse_default_transformation (s:sphere) r =
        match inverse s.default_transformation with
        | Ok idt -> transform r idt
        | Error s -> raise (Exception(s)) //TODO - decide how I want this to propagate through the code; exception is temporary

    let intersect (s:sphere) r =
        let r2 = ray_from_inverse_default_transformation s r        
        let sphereToRay = r2.origin - point 0.0 0.0 0.0
        let a = r2.direction.dotProduct(r2.direction)
        let b = 2.0 * r2.direction.dotProduct(sphereToRay)
        let c = sphereToRay.dotProduct(sphereToRay) - 1.0
        let discriminant = b**2.0 - 4.0 * a * c
        if discriminant < 0.0 then
            Seq.empty<intersection>
        else 
            let t1 = (-b - Math.Sqrt(discriminant)) / (2.0 * a)
            let t2 = (-b + Math.Sqrt(discriminant)) / (2.0 * a)
            seq {
                { t = t1; obj = Sphere s; }; 
                { t = t2; obj = Sphere s; };
            }

    let hit intersections : intersection option = 
        let filtered = intersections |> Seq.filter (fun i -> i.t > 0.0)
        if Seq.isEmpty filtered then
            None
        else 
            let lowest = Seq.minBy (fun i -> i.t) filtered
            Some lowest

    let normal_at (s:sphere) world_point =
        match inverse s.default_transformation with
        | Ok im -> 
            let object_point = im * world_point
            let object_normal = object_point - (point 0.0 0.0 0.0)
            let world_normal = im.Transpose * object_normal
            let v = {
                x = world_normal.x;
                y = world_normal.y;
                z = world_normal.z;
                w = 0.0;
            }
            v.normalize()
        | Error s -> raise (Exception(s))  //TODO - decide how I want this to propagate through the code; exception is temporary
        
        
