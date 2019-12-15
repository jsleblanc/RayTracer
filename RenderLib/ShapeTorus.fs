namespace RenderLib

open System
open System.Numerics
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes

module ShapeTorus = 

    let twopi = 2.0 * Math.PI
    let cpx2 = new Complex(2.0,0.0)
    let cpx3 = new Complex(3.0,0.0)
    let cpx4 = new Complex(4.0,0.0)
    let cpx5 = new Complex(5.0,0.0)
    let cpx6 = new Complex(6.0,0.0)
    let cpx8 = new Complex(8.0,0.0)
    let cpx12 = new Complex(12.0,0.0)
    let cpx16 = new Complex(16.0,0.0)
    let cpx27 = new Complex(27.0,0.0)
    let cpx108 = new Complex(108.0,0.0)
    let cpx256 = new Complex(256.0,0.0)

    let cbrt a n =         
        let rho = Math.Pow(Complex.Abs(a), 1.0/3.0)
        let theta = ((twopi * float n) + a.Phase) / 3.0
        new Complex(rho * Math.Cos(theta), rho * Math.Sin(theta))

    let solveQuadratic a b c =
        if a = Complex.Zero then
            if b = Complex.Zero then
                []
            else
                [-c / b]
        else
            let radicand = b*b - cpx4*a*c
            if radicand = Complex.Zero then
                [-b / (cpx2 * a)]
            else
                let r = Complex.Sqrt(radicand)
                let d = cpx2 * a
                [
                    (-b + r) / d;
                    (-b - r) / d;
                ]

    let solveCubic a b c d =
        if a = Complex.Zero then
            solveQuadratic a b c
        else
            let b = b / a
            let c = c / a
            let d = d / a

            let S = b / cpx3
            let D = c / cpx3 - S*S
            let E = S*S*S + (d - S*c)/cpx2
            let Froot = Complex.Sqrt(E*E + D*D*D)
            let mutable F = -Froot - E
            if F = Complex.Zero then
                F <- Froot - E
            let func i =
                let G = cbrt F i
                G - D/G - S
            [0..3]
            |> List.map func

    let solveQuartic a b c d e =
        if a = Complex.Zero then
            solveCubic b c d e
        else
            let b = b / a
            let c = c / a
            let d = d / a
            let e = e / a
            let b2 = b*b
            let b3 = b*b2
            let b4 = b2*b2
            let alpha = (-cpx3/cpx8)*b2+c
            let beta = b3/cpx8 - b*c/cpx2 + d
            let gamma = (-cpx3/cpx256)*b4 + b2*c/cpx16 - b*d/cpx4 + e
            let alpha2 = alpha * alpha
            let t = -b / cpx4
            if beta = Complex.Zero then
                let rad = Complex.Sqrt(alpha2 - cpx4/gamma)
                let r1 = Complex.Sqrt((-alpha + rad) / cpx2)
                let r2 = Complex.Sqrt((-alpha - rad) / cpx2)
                [
                    t + r1;
                    t - r1;
                    t + r2;
                    t - r2;
                ]
            else
                let alpha3 = alpha * alpha2
                let P = -(alpha2/cpx12 + gamma)
                let Q = -alpha3/cpx108 + alpha*gamma/cpx3 - beta*beta/cpx8
                let R = -Q/cpx2 + Complex.Sqrt(Q*Q/cpx4 + P*P*P/cpx27)
                let U = cbrt R 0
                let mutable y = (-cpx5/cpx6)*alpha + U
                if U = Complex.Zero then
                    y <- y - cbrt Q 0
                else
                    y <- y - P/(cpx3 * U)
                let W = Complex.Sqrt(alpha + cpx2*y)
                let r1 = Complex.Sqrt(-(cpx3*alpha + cpx2*y + cpx2*beta/W))
                let r2 = Complex.Sqrt(-(cpx3*alpha + cpx2*y - cpx2*beta/W))
                [
                    t + (W - r1)/cpx2;
                    t + (W + r1)/cpx2;
                    t + (-W - r2)/cpx2;
                    t + (-W + r2)/cpx2;
                ]

    let filterRealNumbers values tolerance = 
        let func (i:Complex) = 
            if areEqualFloat i.Imaginary 0.0 then
                Some i.Real
            else None
        values
        |> List.map func
        |> List.choose id

    let build rmajor rminor =
        let local_normal_at hit shape pt = 
            let a = 1.0 - (rmajor / Math.Sqrt(pt.x**2.0 + pt.y**2.0))
            (vector (a*pt.x) (a*pt.y) pt.z).normalize()
        let local_intersect shape trail ray = 
            let T = 4.0 * rmajor * rmajor
            let J = ray.direction.magnitude()
            let K = 2.0 * ray.origin.dotProduct ray.direction
            let L = ray.origin.magnitude() + rmajor*rmajor - rminor*rminor
            let G = T * (ray.direction.x**2.0 + ray.direction.y**2.0)
            let H = 2.0 * T * (ray.origin.x*ray.direction.x + ray.origin.y*ray.direction.y)
            let I = T * (ray.origin.x**2.0 + ray.origin.y**2.0)

            let a = new Complex(J*J,0.0)
            let b = new Complex(2.0*J*K, 0.0)
            let c = new Complex(2.0*J*L + K*K - G, 0.0)
            let d = new Complex(2.0*K*L - H, 0.0)
            let e = new Complex(L*L - I, 0.0)

            let solution = solveQuartic a b c d e
            if List.isEmpty solution then
                []
            else
                let SURFACE_TOLERANCE = 1.0e-4;
                let realRoots = filterRealNumbers solution epsilon
                realRoots
                |> List.filter (fun i -> i > SURFACE_TOLERANCE)
                |> List.map (fun t -> Shapes.build_intersection t shape trail)
                |> sort_intersection
        let bounds_of shape = BoundingBoxes.build_default
        build (Torus(rmajor,rminor)) local_intersect local_normal_at bounds_of

