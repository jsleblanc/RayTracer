namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes

module ShapeCube =

    let build =
        let local_normal_at hit shape pt =
            let ax = Math.Abs(pt.x)
            let ay = Math.Abs(pt.y)
            let maxc = [| ax; ay; Math.Abs(pt.z); |] |> Seq.max
            if maxc = ax then
                vector pt.x 0.0 0.0
            else
                if maxc = ay then
                    vector 0.0 pt.y 0.0
                else
                    vector 0.0 0.0 pt.z
        let local_intersect shape trail ray =
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
            let tmin = [| xtmin; ytmin; ztmin; |] |> Seq.max
            let tmax = [| xtmax; ytmax; ztmax; |] |> Seq.min
            if tmin > tmax then
                []
            else
                [
                    Shapes.build_intersection tmin shape trail;
                    Shapes.build_intersection tmax shape trail;
                ]
        let bounds_of shape = BoundingBoxes.build (point -1.0 -1.0 -1.0) (point 1.0 1.0 1.0)
        build Cube local_intersect local_normal_at bounds_of