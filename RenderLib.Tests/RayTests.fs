namespace RenderLib.Tests

open System
open Xunit
open FsCheck
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Ray
open RenderLib.Sphere

module RayTests = 
    
    [<Fact>]
    let ``Creating and querying a ray``() =
        let o = point 1.0 2.0 3.0
        let d = vector 4.0 5.0 6.0
        let r = { origin = o; direction = d; }
        Assert.Equal(o, r.origin)
        Assert.Equal(d, r.direction)

    [<Fact>]
    let ``Computing a point from a distance``() =
        let r = {
            origin = point 2.0 3.0 4.0;
            direction = vector 1.0 0.0 0.0;
        }
        Assert.Equal(point 2.0 3.0 4.0, position r 0.0)
        Assert.Equal(point 3.0 3.0 4.0, position r 1.0)
        Assert.Equal(point 1.0 3.0 4.0, position r -1.0)
        Assert.Equal(point 4.5 3.0 4.0, position r 2.5)

    [<Fact>]
    let ``A ray intersects a sphere at two points``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = sphere()
        let xs = intersect s r
        Assert.Equal(2, xs.Length)
        Assert.Equal(4.0, xs.[0])
        Assert.Equal(6.0, xs.[1])

    [<Fact>]
    let ``A ray intersects a sphere at a tangent``() =
        let r = { origin = point 0.0 1.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = sphere()
        let xs = intersect s r
        Assert.Equal(2, xs.Length)
        Assert.Equal(5.0, xs.[0])
        Assert.Equal(5.0, xs.[1])

    [<Fact>]
    let ``A ray misses a sphere``() =
        let r = { origin = point 0.0 2.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = sphere()
        let xs = intersect s r
        Assert.Equal(0, xs.Length)

    [<Fact>]
    let ``A ray originates inside a sphere``() =
        let r = { origin = point 0.0 0.0 0.0; direction = vector 0.0 0.0 1.0; }
        let s = sphere()
        let xs = intersect s r
        Assert.Equal(2, xs.Length)
        Assert.Equal(-1.0, xs.[0])
        Assert.Equal(1.0, xs.[1])

    [<Fact>]
    let ``A sphere is behind a ray``() =
        let r = { origin = point 0.0 0.0 5.0; direction = vector 0.0 0.0 1.0; }
        let s = sphere()
        let xs = intersect s r
        Assert.Equal(2, xs.Length)
        Assert.Equal(-6.0, xs.[0])
        Assert.Equal(-4.0, xs.[1])