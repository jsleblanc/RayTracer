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

    let shapeWithColor shape color = 
        match shape with
        | Sphere s -> Sphere({ s with material = { s.material with color = color; } })
        | Plane p -> Plane({ p with material = { p.material with color = color; } })

    let shapeWithTransformation shape matrix =
        match shape with
        | Sphere s -> Sphere({ s with default_transformation = matrix; })            
        | Plane p -> Plane({ p with default_transformation = matrix; })

    type intersection = {
        t: float;
        obj: shape;
    }

    let ray_from_inverse_default_transformation (sp:shapeProperties) ray =
        match inverse sp.default_transformation with
        | Ok idt -> transform ray idt
        | Error s -> raise (Exception(s)) //TODO - decide how I want this to propagate through the code; exception is temporary        

    let intersect (shape:shape) r =
        match shape with
        | Sphere s ->
            let r2 = ray_from_inverse_default_transformation s r
            let shapeToRay = r2.origin - point 0.0 0.0 0.0
            let a = r2.direction.dotProduct(r2.direction)
            let b = 2.0 * r2.direction.dotProduct(shapeToRay)
            let c = shapeToRay.dotProduct(shapeToRay) - 1.0
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
            if Math.Abs(r.direction.y) < epsilon then
                Seq.empty<intersection>
            else                
                seq { { t = -r.origin.y / r.direction.y; obj = Plane p; } }

    let hit intersections : intersection option = 
        let filtered = intersections |> Seq.filter (fun i -> i.t > 0.0)
        if Seq.isEmpty filtered then
            None
        else 
            let lowest = Seq.minBy (fun i -> i.t) filtered
            Some lowest

    let normal_at (shape:shape) world_point =
        let local_normal_at sp pt =
            match inverse sp.default_transformation with
            | Ok im -> 
                let local_point = im * pt
                let local_normal = local_point - (point 0.0 0.0 0.0)
                let world_normal = im.Transpose * local_normal
                let v = {
                    x = world_normal.x;
                    y = world_normal.y;
                    z = world_normal.z;
                    w = 0.0;
                }
                v.normalize()
            | Error s -> raise (Exception(s))  //TODO - decide how I want this to propagate through the code; exception is temporary
        match shape with 
        | Sphere sp -> local_normal_at sp world_point
        | Plane _ -> vector 0.0 1.0 0.0
       
    type precomputed = {
        t: float;
        obj: shape;
        point: tuple;
        eyev: tuple;
        normalv: tuple;
        inside: bool;
        over_point: tuple;
    }

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
       