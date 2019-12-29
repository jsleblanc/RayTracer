namespace RenderLib

open System
open Tuple
open Ray
open Shapes

module ShapeCone = 
    
    let build (minimum:float) (maximum:float) closed =
        let local_intersect shape trail ray =
            let afn r = r.direction.x ** 2.0 - r.direction.y ** 2.0 + r.direction.z ** 2.0
            let bfn r = 
                2.0 * r.origin.x * r.direction.x -
                2.0 * r.origin.y * r.direction.y +
                2.0 * r.origin.z * r.direction.z
            let cfn r = r.origin.x ** 2.0 - r.origin.y ** 2.0 + r.origin.z ** 2.0
            ShapeConic.intersect shape trail ray afn bfn cfn (fun (y) -> Math.Abs(y))
        let local_normal_at hit shape pt =
            let nfn pt =
                let y = Math.Sqrt(pt.x ** 2.0 + pt.z ** 2.0)
                let y_p = if (pt.y > 0.0) then -y else y
                vector pt.x y_p pt.z
            ShapeConic.normal_at shape pt nfn
        let bounds_of shape = 
            let a = Math.Abs(minimum)
            let b = Math.Abs(maximum)
            let limit = Math.Max(a,b)
            BoundingBoxes.build (point -limit minimum -limit) (point limit maximum limit)
        let divide shape = shape
        build (Cone(minimum,maximum,closed)) local_intersect local_normal_at bounds_of divide

    let build_default =
        build Double.NegativeInfinity Double.PositiveInfinity false