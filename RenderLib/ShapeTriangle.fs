namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes

module ShapeTriangle = 

    let data tri =
        match tri.shape with
        | Triangle data -> data
        | _ -> failwith "expected a triangle"

    let private precompute (p1:tuple) (p2:tuple) (p3:tuple) =
        let e1 = p2 - p1
        let e2 = p3 - p1
        let normal = e2.crossProduct(e1).normalize()
        (e1, e2, normal)

    let build p1 p2 p3 smooth =
        let (e1, e2, normal) = precompute p1 p2 p3
        let local_normal_at hit shape pt = normal
        let local_intersect shape trail ray =
            let tri = data shape
            let dir_cross_e2 = ray.direction.crossProduct(tri.e2)
            let det = tri.e1.dotProduct(dir_cross_e2)
            if Math.Abs(det) < Common.epsilon then 
                []
            else
                let f = 1.0 / det
                let p1_to_origin = ray.origin - tri.p1
                let u = f * p1_to_origin.dotProduct(dir_cross_e2)
                if u < 0.0 || u > 1.0 then 
                    []
                else
                    let origin_cross_e1 = p1_to_origin.crossProduct(tri.e1)
                    let v = f * ray.direction.dotProduct(origin_cross_e1)
                    if v < 0.0 || (u + v) > 1.0 then
                        []
                    else
                        let t = f * tri.e2.dotProduct(origin_cross_e1)
                        [build_intersection_triangle t shape u v trail;]
        let bounds_of shape = 
            BoundingBoxes.build_default
            |> BoundingBoxes.add_point p1
            |> BoundingBoxes.add_point p2
            |> BoundingBoxes.add_point p3
        let data = {
            p1 = p1;
            p2 = p2; 
            p3 = p3;
            e1 = e1;
            e2 = e2;
            n1 = normal;
            n2 = normal;
            n3 = normal;
            normal = normal;
            smooth = smooth;
        }
        build (Triangle(data)) local_intersect local_normal_at bounds_of