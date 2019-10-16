namespace RenderLib.Tests

open System
open Xunit
open FsCheck
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Ray
open RenderLib.Shapes

module IntersectionTests = 
    
    [<Fact>]
    let ``An intersection encapsulates t and object``() =
        let s = Sphere(shapeProperties.Default)
        let i = {
            t = 3.5;
            obj = s;
        }
        Assert.Equal(s, i.obj)

    [<Fact>]
    let ``The hit, when all intersections have positive t``() =
        let s = Sphere(shapeProperties.Default)
        let i1 = {
            t = 1.0;
            obj = s;
        }
        let i2 = {
            t = 2.0;
            obj = s;
        }
        let xs = seq { i1; i2; }
        match hit xs with
        | Some i -> Assert.Equal(i1, i)
        | None -> Assert.True(false, "Should have returned a value")

    [<Fact>]
    let ``The hit, when some intersections have negative t``() =
        let s = Sphere(shapeProperties.Default)
        let i1 = {
            t = -1.0;
            obj = s;
        }
        let i2 = {
            t = 1.0;
            obj = s;
        }
        let xs = seq { i1; i2; }
        match hit xs with
        | Some i -> Assert.Equal(i2, i)
        | None -> Assert.True(false, "Should have returned a value")

    [<Fact>]
    let ``The hit, when all intersections have negative t``() =
        let s = Sphere(shapeProperties.Default)
        let i1 = {
            t = -1.0;
            obj = s;
        }
        let i2 = {
            t = -2.0;
            obj = s;
        }
        let xs = seq { i1; i2; }
        match hit xs with
        | None -> Assert.True(true)
        | Some i -> Assert.True(false, "Should not have returned a value")

    [<Fact>]
    let ``The hit is always the lowest non-negative intersection``() =
        let s = Sphere(shapeProperties.Default)
        let i1 = {
            t = 5.0;
            obj = s;
        }
        let i2 = {
            t = 7.0;
            obj = s;
        }
        let i3 = {
            t = -3.0;
            obj = s;
        }
        let i4 = {
            t = 2.0;
            obj = s;
        }
        let xs = seq { i1; i2; i3; i4; }
        match hit xs with
        | Some i -> Assert.Equal(i4, i)
        | None -> Assert.True(false, "Should have returned a value")
