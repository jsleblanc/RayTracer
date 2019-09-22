namespace RenderLib.Tests

open System
open Xunit
open FsCheck
open RenderLib.Engine

module TupleTests = 

    type Overrides() =
        static member Float() =
            Arb.Default.Float()
            |> Arb.filter (fun f -> not <| System.Double.IsNaN(f) && not <| System.Double.IsInfinity(f)) 
    
    [<Fact>]
    let ``tuple equality``() =
        let areEqual x y z w = 
            let left = { x = x; y = y; z = z; w = w }
            let right = { x = x; y = y; z = z; w = w }
            left = right
        Check.VerboseThrowOnFailure areEqual

    [<Fact>]
    let ``point() creates tuple with w = 1``() =
        let tupleIsPoint x y z = 
            let t = point x y z
            t.w = 1.0
        Check.QuickThrowOnFailure tupleIsPoint

    [<Fact>]
    let ``vector() creates tuple with w = 0``() =
        let tupleIsVector x y z = 
            let t = vector x y z
            t.w = 0.0
        Check.QuickThrowOnFailure tupleIsVector

    [<Fact>]
    let ``adding two tuples (point + vector = point)``() =
        let addedCorrectly a b c x y z =
            let s1 = (point a b c) + (vector x y z)
            let s2 = point (a + x) (b + y) (c + z)
            s1 = s2
        Check.QuickThrowOnFailure addedCorrectly

    [<Fact>]
    let ``adding two tuples (vector + vector = vector)``() =
        let addedCorrectly a b c x y z =
            let s1 = (vector a b c) + (vector x y z)
            let s2 = vector (a + x) (b + y) (c + z)
            s1 = s2
        Check.QuickThrowOnFailure addedCorrectly

    [<Fact>]
    let ``subtracting tuples (point - point = vector)``() =
        let subtractedCorrectly a b c x y z =
            let s1 = (point a b c) - (point x y z)
            let s2 = vector (a - x) (b - y) (c - z)
            s1 = s2
        Check.QuickThrowOnFailure subtractedCorrectly

    [<Fact>]
    let ``subtracting tuples (point - vector = point)``() =
        let subtractedCorrectly a b c x y z =
            let s1 = (point a b c) - (vector x y z)
            let s2 = point (a - x) (b - y) (c - z)
            s1 = s2
        Check.QuickThrowOnFailure subtractedCorrectly

    [<Fact>]
    let ``subtracting tuples (vector - vector = vector)``() =
        let subtractedCorrectly a b c x y z =
            let s1 = (vector a b c) - (vector x y z)
            let s2 = vector (a - x) (b - y) (c - z)
            s1 = s2
        Check.QuickThrowOnFailure subtractedCorrectly

    [<Fact>]
    let ``subtracting a vector from a zero vector``() =
        let subtractedCorrectly a b c =
            let zero = vector 0.0 0.0 0.0
            let s1 = zero - (vector a b c)
            let s2 = vector -a -b -c
            s1 = s2
        Check.QuickThrowOnFailure subtractedCorrectly

    [<Fact>]
    let ``negating a tuple``() =
        let negatedCorrectly a b c d =
            let t =  { x = a; y = b; z = c; w = d; }
            let s1 = -t
            let s2 = { x = -a; y = -b; z = -c; w = -d; }
            s1 = s2
        Check.QuickThrowOnFailure negatedCorrectly

    [<Fact>]
    let ``multiplying a tuple by a scalar``() =
        let multipliedCorrectly a b c d x =
            let s1 = { x = a; y = b; z = c; w = d; } * x
            let s2 = { x = a * x; y = b * x; z = c * x; w = d * x; }
            s1 = s2
        Check.QuickThrowOnFailure multipliedCorrectly

    [<Fact>]
    let ``dividing a tuple by a scalar``() =
        let dividedCorrectly a b c d x =
            let s1 = { x = a; y = b; z = c; w = d; } / x
            let s2 = { x = a / x; y = b / x; z = c / x; w = d / x; }
            s1 = s2
        Check.QuickThrowOnFailure dividedCorrectly

    [<Theory>]
    [<InlineData(1, 0, 0)>]
    [<InlineData(0, 1, 0)>]
    [<InlineData(0, 0, 1)>]
    let ``computing the magnitude of vector should be 1``(a, b, c) =
        let m = (vector a b c).magnitude()
        Assert.Equal(1.0, m)

    [<Fact>]
    let ``computing the magnitude of vector(1,2,3) should be √14``() =
        let m = (vector 1.0 2.0 3.0).magnitude()
        Assert.Equal(Math.Sqrt(14.0), m)

    [<Fact>]
    let ``computing the magnitude of vector(-1,-2,-3) should be √14``() =
        let m = (vector -1.0 -2.0 -3.0).magnitude()
        Assert.Equal(Math.Sqrt(14.0), m)

    [<Fact>]
    let ``computing the magnitude of vector``() =
        Arb.register<Overrides>()
        let magnitudeCorrectly a b c =
            let s1 = (vector a b c).magnitude()
            let s2 = Math.Sqrt(a**2.0 + b**2.0 + c**2.0)
            s1 = s2
        Check.QuickThrowOnFailure magnitudeCorrectly

    [<Fact>]
    let ``normalizing vector(4,0,0) gives vector(1,0,0)``() =        
        let v = vector 4.0 0.0 0.0
        let n = v.normalize()
        let expected = vector 1.0 0.0 0.0
        Assert.Equal(expected, n)

    [<Fact>]
    let ``normalizing vector``() =  
        let f a b c =
            let v = vector a b c
            let n1 = v.normalize();
            let n2 = {
                x = v.x / v.magnitude();
                y = v.y / v.magnitude();
                z = v.z / v.magnitude();
                w = v.w / v.magnitude();
            }
            n1 = n2
        Check.QuickThrowOnFailure f

    [<Fact>]
    let ``magnitude of a normalized vector should be 1``() =
        Arb.register<Overrides>()    
        let f a b c = 
            let v = vector a b c
            (v.normalize().magnitude() - 1.0) <= epsilon
        Check.QuickThrowOnFailure f

    [<Fact>]
    let ``dot product of two vectors``() =
        let v1 = vector 1.0 2.0 3.0
        let v2 = vector 2.0 3.0 4.0
        let d1 = v1.dotProduct(v2)
        let d2 = v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w
        Assert.True(Math.Abs(d1 - d2) <= epsilon)

    [<Fact>]
    let ``cross product of two vectors``() =
        let v1 = vector 1.0 2.0 3.0
        let v2 = vector 2.0 3.0 4.0
        let c1 = v1.crossProduct(v2)
        let c2 = v2.crossProduct(v1)
        Assert.Equal((vector -1.0 2.0 -1.0), c1)
        Assert.Equal((vector 1.0 -2.0 1.0), c2)