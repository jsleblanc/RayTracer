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
        trail:seq<shape>;
        u:float;
        v:float;
    }
    and intersect_t = shape -> shape seq -> ray -> intersection seq
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

    let build_intersection t shape trail =
        { t = t; obj = shape; trail = trail; u = 0.0; v = 0.0; }

    let private sort_intersection (xs:seq<intersection>) =
        xs |> Seq.sortBy (fun (a) -> a.t)

    let hit (xs:seq<intersection>) =
        let sorted = sort_intersection xs
        sorted |> Seq.tryFind (fun (i) -> i.t > 0.0)

    let transform shape transform =
        let inverse_transform = inverse transform
        let inverse_transpose = inverse_transform.Transpose
        { shape with inverse_transform = inverse_transform; inverse_transpose_transform = inverse_transpose; }

    let texture shape material =
        { shape with material = Some material; }

    let intersect (shape:shape) trail ray =
        let ray2 = Ray.transform ray shape.inverse_transform
        shape.local_intersect shape trail ray2


        