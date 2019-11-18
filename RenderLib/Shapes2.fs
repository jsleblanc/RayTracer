namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open BoundingBoxes

module Shapes2 = 

    type tri_data = {
        p1: tuple;
        p2: tuple;
        p3: tuple;
        n1: tuple;
        n2: tuple;
        n3: tuple;
        smooth: bool;
    }
    
    [<CustomEquality; NoComparison>]
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
    } with 
        override this.Equals(other) = 
            match other with
            | :? shape as other ->
                this.shape = other.shape &&
                this.transform = other.transform &&
                this.inverse_transform = other.inverse_transform &&
                this.inverse_transpose_transform = other.inverse_transpose_transform &&
                this.material = other.material &&
                this.shadow = other.shadow
            | _ -> Object.Equals(this, other)
        override this.GetHashCode() = 0
    and shape_t =
    | Plane
    | Sphere
    | Cube
    | Cylinder of Minimum:float * Maximum:float * Closed:bool
    | Cone of Minimum:float * Maximum:float * Closed:bool
    | Triangle of tri_data
    | Group of Children:shape list
    and intersection = {
        t:float;
        obj:shape;
        trail:shape list;
        u:float;
        v:float;
    }
    and intersect_t = shape -> shape list -> ray -> intersection list
    and normal_t = intersection option -> shape -> tuple -> tuple
    and bounds_of_t = shape -> boundingBox_t

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

    let sort_intersection (xs:intersection list) =
        xs |> List.sortBy (fun (a) -> a.t)

    let hit (xs:intersection list) =
        let sorted = sort_intersection xs
        sorted |> List.tryFind (fun (i) -> i.t >= 0.0)

    let transform transform shape =
        let inverse_transform = inverse transform
        let inverse_transpose = inverse_transform.Transpose
        { shape with inverse_transform = inverse_transform; inverse_transpose_transform = inverse_transpose; }

    let texture material shape =
        { shape with material = Some material; }

    let materialOrDefault shape = 
        match shape.material with
        | None -> Material.material.Default
        | Some m -> m

    let intersect (shape:shape) trail ray =
        let ray2 = Ray.transform ray shape.inverse_transform
        shape.local_intersect shape trail ray2

    let material trail shape =
        let rec loop head tail =
            match head.material with
            | None -> 
                match tail with
                | [] -> material.Default
                | head :: tail -> loop head tail
            | Some m -> m
        loop shape trail
        
    let world_to_object shape trail wpoint  =
        let rec loop point t = 
            match t with
            | [] -> point
            | parent :: parents ->
                parent.inverse_transform * (loop point parents)
        in
        loop wpoint (shape :: trail)

    let normal_to_world shape trail (normal:tuple) =
        let rec loop (normal:tuple) t =
            match t with
            | [] -> normal
            | shape :: shapes ->
                let n = shape.inverse_transpose_transform * normal
                let n_p = (vector n.x n.y n.z).normalize() in
                loop n_p shapes
        in
        loop normal (shape :: trail)

    let normal_at hit shape trail wpoint =
        let object_point = world_to_object shape trail wpoint in
        let object_normal = shape.local_normal_at hit shape object_point in
        normal_to_world shape trail object_normal