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

    let cbrt a n = 
        let twopi = 2.0 * Math.PI
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
            let c2 = new Complex(2.0,0.0)
            let c4 = new Complex(4.0,0.0)
            let radicand = b*b - c4*a*c
            if radicand = Complex.Zero then
                [-b / (c2 * a)]
            else
                let r = Complex.Sqrt(radicand)
                let d = c2 * a
                [
                    (-b + r) / d;
                    (-b - r) / d;
                ]

    let solveCubic a b c d =
        if a = Complex.Zero then
            solveQuadratic a b c
        else
            let c2 = new Complex(2.0,0.0)
            let c3 = new Complex(3.0,0.0)
            let b = b / a
            let c = c / a
            let d = d / a

            let S = b / c3
            let D = c / c3 - S*S
            let E = S*S*S + (d - S*c)/c2
            let Froot = Complex.Sqrt(E*E + D*D*D)
            let mutable F = -Froot - E
            if F = Complex.Zero then
                F <- Froot - E
            let func i =
                let G = cbrt F i
                G - D/G - S
            [0..3]
            |> List.map func

    let private solve2 (coefficients:float list) = 
        let p = coefficients.[1] / (2.0 * coefficients.[2])
        let q = coefficients.[0] / coefficients.[2]
        let d = p * p - q
        if areEqualFloat d 0.0 then
            [-p]
        else
            if d < 0.0 then
                []
            else
                let sqrt_d = Math.Sqrt(d)
                [sqrt_d - p;-sqrt_d - p;]


    let private solve3 (coefficients:float list) = 
        let a = coefficients.[2] / coefficients.[3]
        let b = coefficients.[1] / coefficients.[3]
        let c = coefficients.[0] / coefficients.[3]
        let sq_a = a**2.0
        let p = 1.0 / 3.0 * (-1.0 / 3.0 * sq_a + b)
        let q = 1.0 / 2.0 * (2.0 / 27.0 * a * sq_a - 1.0 / 3.0 * a * b + c)
        let cb_p = p**3.0
        let d = q * q + cb_p
        let s =
            if areEqualFloat d 0.0 then
                if areEqualFloat q 0.0 then
                    [0.0]
                else
                    let u = Math.Cbrt(-q)
                    [2.0 * u; -u]
            else
                if d < 0.0 then
                    let phi = 1.0 / 3.0 * Math.Acos(-q / Math.Sqrt(-cb_p))
                    let t = 2.0 * Math.Sqrt(-p)
                    [
                        t * Math.Cos(phi);
                        -t * Math.Cos(phi + Math.PI / 3.0);
                        -t * Math.Cos(phi - Math.PI / 3.0);
                    ]
                else
                    let sqrt_d = Math.Sqrt(d)
                    let u = Math.Cbrt(sqrt_d - q)
                    let v = -Math.Cbrt(sqrt_d + q)
                    [u + v]
        let sub = 1.0 / 3.0 * a
        s |> List.map (fun t -> t - sub)

    let private solve4 (coefficients:float list) = 
        let a = coefficients.[3] / coefficients.[4]
        let b = coefficients.[2] / coefficients.[4]
        let c = coefficients.[1] / coefficients.[4]
        let d = coefficients.[0] / coefficients.[4]
        let sq_a = a**2.0
        let p = -3.0 / 8.0 * sq_a + b
        let q = 1.0 / 8.0 * sq_a - 1.0 / 2.0 * a * b + c
        let r = -3.0 / 256.0 * sq_a * sq_a + 1.0 / 16.0 * sq_a * b - 1.0 / 4.0 * a * c + d
        let s = 
            if areEqualFloat r 0.0 then
                let coeffs = [q;p;0.0;1.0;]
                let s = solve3 coeffs
                [s.[0]]
            else
                let coeffs = [
                    1.0 / 2.0 * r * p - 1.0 / 8.0 * q * q;
                    -r;
                    - 1.0 / 2.0 * p;
                    1.0;
                ]
                let s = solve3 coeffs
                let z = s.[0]
                let u = z * z - r
                let v = 2.0 * z - p
                let func x =
                    if areEqualFloat x 0.0 then
                        0.0
                    else
                        if x > 0.0 then
                            Math.Sqrt(x)
                        else
                            0.0                
                let u = func u
                let v = func v
                let coeffs = [
                    z - u;
                    if q < 0.0 then -v else v;
                    1.0;
                ]
                let s1 = solve2 coeffs
                let coeffs = [
                    z + u;
                    if q < 0.0 then v else -v;
                    1.0;
                ]
                let s2 = solve2 coeffs
                s1 @ s2
        let sub = 1.0 / 4.0 * a
        s |> List.map (fun t -> t - sub)

    let build rmajor rminor =
        let local_normal_at hit shape pt = 
            let paramSquared = rmajor**2.0 + rminor**2.0
            let x = pt.x
            let y = pt.y
            let z = pt.z
            let sumSquared = x * x + y * y + z * z
            let vx = 4.0 * x * (sumSquared - paramSquared)
            let vy = 4.0 * y * (sumSquared - paramSquared + 2.0 * rmajor**2.0)
            let vz = 4.0 * z * (sumSquared - paramSquared)
            (vector vx vy vz).normalize()            
        let local_intersect shape trail ray = 
            let ox = ray.origin.x;
            let oy = ray.origin.y;
            let oz = ray.origin.z;
            let dx = ray.direction.x;
            let dy = ray.direction.y;
            let dz = ray.direction.z;
            let sum_d_sqrd = dx * dx + dy * dy + dz * dz;
            let e = ox * ox + oy * oy + oz * oz - rmajor**2.0 - rminor**2.0
            let f = ox * dx + oy * dy + oz * dz;
            let four_a_sqrd = 4.0 * rmajor**2.0
            let coeffs = [
                e * e - four_a_sqrd * (rminor**2.0 - oy * oy);
                4.0 * f * e + 2.0 * four_a_sqrd * oy * dy;
                2.0 * sum_d_sqrd * e + 4.0 * f * f + four_a_sqrd * dy * dy;
                4.0 * sum_d_sqrd * f;
                sum_d_sqrd * sum_d_sqrd;
            ]
            let solution = solve4 coeffs
            if List.isEmpty solution then
                []
            else
                //let intersections = solution |> List.map (fun t -> Shapes.build_intersection t shape trail)
                //sort_intersection intersections
                
                let func mint t = 
                    if t > epsilon && t < mint then t else mint
                let t = List.fold func Double.PositiveInfinity solution
                if Double.IsInfinity(t) then
                    []
                else
                    [Shapes.build_intersection t shape trail]
                
        let bounds_of shape = BoundingBoxes.build_default
        build (Torus(rmajor,rminor)) local_intersect local_normal_at bounds_of

