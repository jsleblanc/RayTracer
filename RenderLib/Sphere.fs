namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray

module Sphere = 

    type sphere(id : Guid) = 
        member this.id = id
        new() = sphere(Guid.NewGuid())

    let intersect s r =
        let sphereToRay = r.origin - point 0.0 0.0 0.0
        let a = r.direction.dotProduct(r.direction)
        let b = 2.0 * r.direction.dotProduct(sphereToRay)
        let c = sphereToRay.dotProduct(sphereToRay) - 1.0
        let discriminant = b**2.0 - 4.0 * a * c
        if discriminant < 0.0 then
            List.empty<float>
        else 
            let t1 = (-b - Math.Sqrt(discriminant)) / (2.0 * a)
            let t2 = (-b + Math.Sqrt(discriminant)) / (2.0 * a)
            [ t1; t2; ]