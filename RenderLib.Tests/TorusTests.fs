namespace RenderLib.Tests

open Xunit
open System
open System.Numerics
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Shapes
open RenderLib.Ray
open RenderLib.Translations

module TorusTests = 

    let cpx f = new Complex(f, 0.0)

    let validatePolynomials order (poly:Complex []) root =
        let mutable power = Complex.One
        let mutable sum = Complex.Zero
        for i in 0 .. order - 1 do
            sum <- sum + poly.[i] * power
            power <- power * root
        if areEqualComplex sum Complex.Zero then true else false

    let checkRoots numRoots (known:Complex list) (found:Complex list) =         
        Assert.Equal(numRoots, List.length found)
        let hasRoot r v = List.exists (fun t -> areEqualComplex r t) v
        Assert.True(known |> List.exists (fun k -> hasRoot k found))        

    [<Fact>]
    let ``Test known quadratic roots``() =
        let test M K L =
           let a = M;
           let b = -M*(K+L);
           let c = M*K*L;
           let poly = [|c;b;a;|]
           Assert.True(validatePolynomials 3 poly K)
           Assert.True(validatePolynomials 3 poly L)
           let rootsFound = ShapeTorus.solveQuadratic a b c
           let expectedRootsFound = if K-L = Complex.Zero then 1 else 2
           Assert.Equal(expectedRootsFound, List.length rootsFound)
           let known = [K;L;]
           checkRoots expectedRootsFound known rootsFound
        test (new Complex(-2.3,+4.8)) (new Complex(+3.2,-4.1)) (new Complex(-2.5,+7.7))
        test (new Complex(+5.5,+4.4)) (new Complex(+8.2,-2.1)) (new Complex(+8.2,-2.1))

    [<Fact>]
    let ``Test known cubic roots``() =
        let test M K L N = 
            let a = M;
            let b = -M*(K+L+N);
            let c = M*(K*L + N*K + N*L);
            let d = -M*K*L*N;
            let poly = [|d;c;b;a|]
            Assert.True(validatePolynomials 4 poly K)
            Assert.True(validatePolynomials 4 poly L)
            Assert.True(validatePolynomials 4 poly N)
            let rootsFound = ShapeTorus.solveCubic a b c d
            let expectedRootsFound = 3
            Assert.Equal(expectedRootsFound, List.length rootsFound)
            let known = [K;L;N;]
            checkRoots expectedRootsFound known rootsFound
        test (cpx 1.0) (cpx 2.0) (cpx 3.0) (cpx 4.0)
        test (new Complex(-2.3,+4.8)) (new Complex(+3.2,-4.1)) (new Complex(-2.5,+7.7)) (new Complex(53.0,-23.9))

    [<Fact>]
    let ``Test known quartic roots``() =
        let test m a b c d = 
            let A = m
            let B = -m*(a + b + c + d)
            let C = m*(a*b + c*d + (a + b)*(c + d))
            let D = -m*(c*d*(a + b) + a*b*(c + d))
            let E = m*a*b*c*d
            let poly = [|E;D;C;B;A;|]
            Assert.True(validatePolynomials 5 poly a)
            Assert.True(validatePolynomials 5 poly b)
            Assert.True(validatePolynomials 5 poly c)
            Assert.True(validatePolynomials 5 poly d)
            let rootsFound = ShapeTorus.solveQuartic A B C D E
            let expectedRootsFound = 4
            Assert.Equal(expectedRootsFound, List.length rootsFound)
            let known = [a;b;c;d;]
            checkRoots expectedRootsFound known rootsFound
        test (cpx 1.0) (cpx 2.0) (cpx 3.0) (cpx 4.0) (cpx 5.0)
        test (cpx 1.0) (cpx 3.2) (cpx -2.5) (cpx 53.0) (cpx -8.7)
        test (new Complex(-2.3,+4.8)) (new Complex(+3.2,-4.1)) (new Complex(-2.5,+7.7)) (new Complex(53.0,-23.9)) (new Complex(-9.2,-8.7))

    //When t is greater than 0, the point is located in front of the ray's origin (looking down the ray's direction), when t is equal to 0, the point coincides with the ray's origin (O), and when t is negative the point is located behind its origin.

    [<Fact>]
    let ``A ray misses a torus``() =
        let t = ShapeTorus.build 4.0 1.0
        let ray = {
            origin = point -2.5 -3.0 0.0;
            direction = vector 1.0 0.0 0.0;
        }
        let xs = intersect t [] ray
        Assert.Equal(0, List.length xs)

    [<Fact>]
    let ``A ray intersects a torus once``() =
        let t = ShapeTorus.build 4.0 1.0
        let ray = {
            origin = point -2.5 -2.0 0.0;
            direction = vector 1.0 0.0 0.0;
        }
        let xs = intersect t [] ray
        Assert.Equal(1, List.length xs)
        Assert.Equal(2.5, xs.[0].t)

    [<Fact>]
    let ``A ray intersects a torus twice``() =
        let t = ShapeTorus.build 4.0 1.0
        let ray = {
            origin = point -2.5 -1.5 0.0;
            direction = vector 1.0 0.0 0.0;
        }
        let xs = intersect t [] ray
        Assert.Equal(2, List.length xs)
        Assert.Equal(1.177, xs.[0].t)
        Assert.Equal(3.823, xs.[1].t)

    [<Fact>]
    let ``A ray intersects a torus three times``() =
        let t = ShapeTorus.build 4.0 1.0
        let ray = {
            origin = point -2.5 -1.0 0.0;
            direction = vector 1.0 0.0 0.0;
        }
        let xs = intersect t [] ray
        Assert.Equal(3, List.length xs)
        Assert.Equal(0.768, xs.[0].t)
        Assert.Equal(2.5, xs.[1].t)
        Assert.Equal(3.268, xs.[2].t)

    [<Fact>]
    let ``A ray intersects a torus four times``() =
        let t = ShapeTorus.build 4.0 1.0
        let ray = {
            origin = point -2.5 0.0 0.0;
            direction = vector 1.0 0.0 0.0;
        }
        let xs = intersect t [] ray
        Assert.Equal(4, List.length xs)
        Assert.Equal(0.5, xs.[0].t)
        Assert.Equal(1.5, xs.[1].t)
        Assert.Equal(2.5, xs.[2].t)
        Assert.Equal(4.5, xs.[3].t)