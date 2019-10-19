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

    let private shapeToProperties shape =
        match shape with
        | Sphere s -> s
        | Plane p -> p

    let private local_normal_at shape pt =
        match shape with
        | Sphere s -> pt - (point 0.0 0.0 0.0)
        | Plane p -> vector 0.0 1.0 0.0

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

    let intersect (s:shape) r =
        let sp = shapeToProperties s
        let idt = inverse sp.default_transformation
        let r2 = transform r idt
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

    let normal_at (shape:shape) pt =
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
       