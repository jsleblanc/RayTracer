namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes2

module ShapePlane =

    let build = 
        let local_normal_at hit shape pt = vector 0.0 1.0 0.0
        let local_intersect shape trail ray =
            if Math.Abs(ray.direction.y) < epsilon then
                []
            else
                [Shapes2.build_intersection (-ray.origin.y / ray.direction.y) shape trail;]
        let bounds_of shape = { minimum = point Double.NegativeInfinity 0.0 Double.NegativeInfinity; maximum = point Double.PositiveInfinity 0.0 Double.PositiveInfinity; }
        build Plane local_intersect local_normal_at bounds_of