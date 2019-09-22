namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Engine

module TupleTests = 
    
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
            t.w = 1
        Check.QuickThrowOnFailure tupleIsPoint

    [<Fact>]
    let ``vector() creates tuple with w = 0``() =
        let tupleIsVector x y z = 
            let t = vector x y z
            t.w = 0
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
