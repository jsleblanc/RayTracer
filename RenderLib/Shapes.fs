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

    let shapeWithColor shape color = 
        match shape with
        | Sphere s ->
            let m = { s.material with color = color; }
            Sphere({ s with material = m})

    let shapeWithTransformation shape matrix =
        match shape with
        | Sphere s -> Sphere({ s with default_transformation = matrix; })            

    type intersection = {
        t: float;
        obj: shape;
    }

    let ray_from_inverse_default_transformation (s:shape) r =
        match s with
        | Sphere sp ->
            match inverse sp.default_transformation with
            | Ok idt -> transform r idt
            | Error s -> raise (Exception(s)) //TODO - decide how I want this to propagate through the code; exception is temporary

    let intersect (s:shape) r =
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

    let normal_at (s:shape) world_point =
        match s with 
        | Sphere sp ->
            match inverse sp.default_transformation with
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
       