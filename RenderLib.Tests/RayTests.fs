namespace RenderLib.Tests

open System
open Xunit
open FsCheck
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Ray
open RenderLib.Translations
open RenderLib.Shapes

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
        let s = Sphere (sphere())
        let xs = intersect s r
        Assert.Equal(2, Seq.length xs)
        Assert.Equal(4.0, (Seq.item 0 xs).t)
        Assert.Equal(6.0, (Seq.item 1 xs).t)

    [<Fact>]
    let ``A ray intersects a sphere at a tangent``() =
        let r = { origin = point 0.0 1.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = Sphere (sphere())
        let xs = intersect s r
        Assert.Equal(2, Seq.length xs)
        Assert.Equal(5.0, (Seq.item 0 xs).t)
        Assert.Equal(5.0, (Seq.item 1 xs).t)

    [<Fact>]
    let ``A ray misses a sphere``() =
        let r = { origin = point 0.0 2.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = Sphere (sphere())
        let xs = intersect s r
        Assert.Equal(0, Seq.length xs)

    [<Fact>]
    let ``A ray originates inside a sphere``() =
        let r = { origin = point 0.0 0.0 0.0; direction = vector 0.0 0.0 1.0; }
        let s = Sphere (sphere())
        let xs = intersect s r
        Assert.Equal(2, Seq.length xs)
        Assert.Equal(-1.0, (Seq.item 0 xs).t)
        Assert.Equal(1.0, (Seq.item 1 xs).t)

    [<Fact>]
    let ``A sphere is behind a ray``() =
        let r = { origin = point 0.0 0.0 5.0; direction = vector 0.0 0.0 1.0; }
        let s = Sphere (sphere())
        let xs = intersect s r
        Assert.Equal(2, Seq.length xs)
        Assert.Equal(-6.0, (Seq.item 0 xs).t)
        Assert.Equal(-4.0, (Seq.item 1 xs).t)

    [<Fact>]
    let ``Translating a ray``() =
        let r = { origin = point 1.0 2.0 3.0; direction = vector 0.0 1.0 0.0; }
        let m = translation 3.0 4.0 5.0
        let r2 = transform r m       
        Assert.Equal(point 4.0 6.0 8.0, r2.origin)
        Assert.Equal(vector 0.0 1.0 0.0, r2.direction)        

    [<Fact>]
    let ``Scaling a ray``() =
        let r = { origin = point 1.0 2.0 3.0; direction = vector 0.0 1.0 0.0; }
        let m = scaling 2.0 3.0 4.0
        let r2 = transform r m
        Assert.Equal(point 2.0 6.0 12.0, r2.origin)
        Assert.Equal(vector 0.0 3.0 0.0, r2.direction)
