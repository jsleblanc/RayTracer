namespace RenderLib

open System
open Tuple
open Ray
open Shapes

module ShapeCylinder =

    let build minimum maximum closed =
        let local_intersect shape trail ray =
            let afn r = r.direction.x ** 2.0 + r.direction.z ** 2.0
            let bfn r = 2.0 * r.origin.x * r.direction.x + 2.0 * r.origin.z * r.direction.z
            let cfn r = r.origin.x ** 2.0 + r.origin.z ** 2.0 - 1.0
            ShapeConic.intersect shape trail ray afn bfn cfn (fun _ -> 1.0)
        let local_normal_at hit shape pt =
            let nfn point = vector point.x 0.0 point.z
            ShapeConic.normal_at shape pt nfn
        let bounds_of shape = BoundingBoxes.build (point -1.0 minimum -1.0) (point 1.0 maximum 1.0)
        let divide shape = shape
        build (Cylinder(minimum,maximum,closed)) local_intersect local_normal_at bounds_of divide

    let build_default =
        build Double.NegativeInfinity Double.PositiveInfinity false