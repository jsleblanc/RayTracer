namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray

module Shapes = 

    type sphere(id : Guid) = 
        member this.id = id
        new() = sphere(Guid.NewGuid())

    type shape =
    | Sphere of sphere

    type intersection = {
        t: float;
        obj: shape;
    }

    let intersect s r =
        let sphereToRay = r.origin - point 0.0 0.0 0.0
        let a = r.direction.dotProduct(r.direction)
        let b = 2.0 * r.direction.dotProduct(sphereToRay)
        let c = sphereToRay.dotProduct(sphereToRay) - 1.0
        let discriminant = b**2.0 - 4.0 * a * c
        if discriminant < 0.0 then
            Seq.empty<intersection>
        else 
            let t1 = (-b - Math.Sqrt(discriminant)) / (2.0 * a)
            let t2 = (-b + Math.Sqrt(discriminant)) / (2.0 * a)
            seq {
                { t = t1; obj = s; }; 
                { t = t2; obj = s; };
            }

    let hit intersections : intersection option = 
        let filtered = intersections |> Seq.filter (fun i -> i.t > 0.0)
        if Seq.isEmpty filtered then
            None
        else 
            let lowest = Seq.minBy (fun i -> i.t) filtered
            Some lowest