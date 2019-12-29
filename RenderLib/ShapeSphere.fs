namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes

module ShapeSphere = 

    let build =
        let local_normal_at hit shape pt = vector pt.x pt.y pt.z
        let local_intersect shape trail ray =
            let normal = local_normal_at None shape ray.origin
            let a = ray.direction.dotProduct(ray.direction)
            let b = 2.0 * ray.direction.dotProduct(normal)
            let c = normal.dotProduct(normal) - 1.0
            let discriminant = b**2.0 - 4.0 * a * c
            if discriminant < 0.0 then
                []
            else 
                let t1 = (-b - Math.Sqrt(discriminant)) / (2.0 * a)
                let t2 = (-b + Math.Sqrt(discriminant)) / (2.0 * a)
                [
                    Shapes.build_intersection t1 shape trail;
                    Shapes.build_intersection t2 shape trail;
                ]
        let bounds_of shape = BoundingBoxes.build (point -1.0 -1.0 -1.0) (point 1.0 1.0 1.0)
        let divide shape = shape
        build Sphere local_intersect local_normal_at bounds_of divide
