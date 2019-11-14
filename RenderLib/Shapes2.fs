namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material

module Shapes2 = 

    type boundingBox = {
        minimum: tuple;
        maximum: tuple;
    } with static member Default = {
            minimum = point Double.PositiveInfinity Double.PositiveInfinity Double.PositiveInfinity;
            maximum = point Double.NegativeInfinity Double.NegativeInfinity Double.NegativeInfinity;
        }

    type tri_data = {
        p1: tuple;
        p2: tuple;
        p3: tuple;
        n1: tuple;
        n2: tuple;
        n3: tuple;
        smooth: bool;
    }
    
    type shape = {
        shape:shape_t;
        transform:matrix;
        inverse_transform:matrix;
        inverse_transpose_transform:matrix;
        material:material option;
        local_intersect:intersect_t;
        local_normal_at:normal_t;
        bounds_of:bounds_of_t;
        shadow: bool;
    }
    and shape_t =
    | Plane
    | Sphere
    | Cube
    | Cylinder of Minimum:float * Maximum:float * Closed:bool
    | Cone of Minimum:float * Maximum:float * Closed:bool
    | Triangle of tri_data
    | Group of Children:seq<shape>
    and intersection = {
        t: float;
        obj: shape;
    }
    and intersect_t = shape -> ray -> intersection seq
    and normal_t = shape -> tuple -> tuple
    and bounds_of_t = shape -> boundingBox

    let build (shape:shape_t) (isect:intersect_t) (normal:normal_t) (bounds_of:bounds_of_t) = {
        shape = shape;
        transform = matrix.identity_matrix;
        inverse_transform = matrix.identity_matrix;
        inverse_transpose_transform = matrix.identity_matrix;
        material = None;
        local_intersect = isect;
        local_normal_at = normal;
        bounds_of = bounds_of;
        shadow = true;
    }

    let transform shape transform =
        let inverse_transform = inverse transform
        let inverse_transpose = inverse_transform.Transpose
        { shape with inverse_transform = inverse_transform; inverse_transpose_transform = inverse_transpose; }

    let texture shape material =
        { shape with material = Some material; }

    module Sphere =
        
        let build =
            let local_normal_at shape pt = pt - (point 0.0 0.0 0.0)
            let local_intersect shape ray =
                let normal = local_normal_at shape ray.origin
                let a = ray.direction.dotProduct(ray.direction)
                let b = 2.0 * ray.direction.dotProduct(normal)
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
            let bounds_of shape = { minimum = point -1.0 -1.0 -1.0; maximum = point 1.0 1.0 1.0; }
            build Sphere local_intersect local_normal_at bounds_of

    module Cube =
        
        let build =
            let local_normal_at shape pt =
                let ax = Math.Abs(pt.x)
                let ay = Math.Abs(pt.y)
                let maxc = seq { ax; ay; Math.Abs(pt.z); } |> Seq.max
                if maxc = ax then
                    vector pt.x 0.0 0.0
                else
                    if maxc = ay then
                        vector 0.0 pt.y 0.0
                    else
                        vector 0.0 0.0 pt.z
            let local_intersect shape ray =
                let check_axis origin (direction:float) min max = 
                    let tmin_numerator = min - origin
                    let tmax_numerator = max - origin
                    let mutable tmin = 0.0
                    let mutable tmax = 0.0
                    if Math.Abs(direction) >= epsilon then
                        tmin <- tmin_numerator / direction
                        tmax <- tmax_numerator / direction
                    else
                        tmin <- tmin_numerator * Double.PositiveInfinity
                        tmax <- tmax_numerator * Double.PositiveInfinity
                    swapIfGreater tmin tmax
                let (xtmin, xtmax) = check_axis ray.origin.x ray.direction.x -1.0 1.0
                let (ytmin, ytmax) = check_axis ray.origin.y ray.direction.y -1.0 1.0
                let (ztmin, ztmax) = check_axis ray.origin.z ray.direction.z -1.0 1.0
                let tmin = seq { xtmin; ytmin; ztmin; } |> Seq.max
                let tmax = seq { xtmax; ytmax; ztmax; } |> Seq.min
                if tmin > tmax then
                    Seq.empty<intersection>
                else
                    seq {
                        { t = tmin; obj = shape; };
                        { t = tmax; obj = shape; };
                    }
            let bounds_of shape = { minimum = point -1.0 -1.0 -1.0; maximum = point 1.0 1.0 1.0; }
            build Cube local_intersect local_normal_at bounds_of

    module Plane =
        
        let build = 
            let local_normal_at shape pt = vector 0.0 1.0 0.0
            let local_intersect shape ray =
                if Math.Abs(ray.direction.y) < epsilon then
                    Seq.empty<intersection>
                else
                    seq [{ t = -ray.origin.y / ray.direction.y; obj = shape; }]
            let bounds_of shape = { minimum = point Double.NegativeInfinity 0.0 Double.NegativeInfinity; maximum = point Double.PositiveInfinity 0.0 Double.PositiveInfinity; }
            build Plane local_intersect local_normal_at bounds_of

    module Conic =
        
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