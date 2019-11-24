namespace RenderLib.Tests

open System
open Xunit
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Ray
open RenderLib.PreparedComputations
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Material

module IntersectionTests = 
    
    [<Fact>]
    let ``An intersection encapsulates t and object``() =
        let s = ShapeSphere.build
        let i = Shapes.build_intersection 3.5 s []
        Assert.Equal(s, i.obj)

    [<Fact>]
    let ``The hit, when all intersections have positive t``() =
        let s = ShapeSphere.build
        let i1 = Shapes.build_intersection 1.0 s []
        let i2 = Shapes.build_intersection 2.0 s []
        let xs = [ i1; i2; ]
        match hit_d xs with
        | Some i -> Assert.Equal(i1, i)
        | None -> Assert.True(false, "Should have returned a value")

    [<Fact>]
    let ``The hit, when some intersections have negative t``() =
        let s = ShapeSphere.build
        let i1 = Shapes.build_intersection -1.0 s []
        let i2 = Shapes.build_intersection 1.0 s []
        let xs = [ i1; i2; ]
        match hit_d xs with
        | Some i -> Assert.Equal(i2, i)
        | None -> Assert.True(false, "Should have returned a value")

    [<Fact>]
    let ``The hit, when all intersections have negative t``() =
        let s = ShapeSphere.build
        let i1 = Shapes.build_intersection -1.0 s []
        let i2 = Shapes.build_intersection -2.0 s []
        let xs = [ i1; i2; ]
        match hit_d xs with
        | None -> Assert.True(true)
        | Some i -> Assert.True(false, "Should not have returned a value")

    [<Fact>]
    let ``The hit is always the lowest non-negative intersection``() =
        let s = ShapeSphere.build
        let i1 = Shapes.build_intersection 5.0 s []
        let i2 = Shapes.build_intersection 7.0 s []
        let i3 = Shapes.build_intersection -3.0 s []
        let i4 = Shapes.build_intersection 2.0 s []
        let xs = [ i1; i2; i3; i4; ]
        match hit_d xs with
        | Some i -> Assert.Equal(i4, i)
        | None -> Assert.True(false, "Should have returned a value")

    [<Fact>]
    let ``Precomputing the state of an intersection``() =
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = ShapeSphere.build
        let i = Shapes.build_intersection 4.0 s []
        let comps = prepare i r [i]
        Assert.Equal(i.t, comps.t)
        Assert.Equal(point 0.0 0.0 -1.0, comps.point)
        Assert.Equal(vector 0.0 0.0 -1.0, comps.eyev)
        Assert.Equal(vector 0.0 0.0 -1.0, comps.normalv)

    [<Fact>]
    let ``The hit, when an intersection occurs on the outside``() =
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = ShapeSphere.build
        let i = Shapes.build_intersection 4.0 s []
        let comps = prepare i r [i]
        Assert.False(comps.inside)

    [<Fact>]
    let ``The hit, when an intersection occurs on the inside``() =
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = ShapeSphere.build
        let i = Shapes.build_intersection 1.0 s []
        let comps = prepare i r [i]
        Assert.Equal(point 0.0 0.0 1.0, comps.point)
        Assert.Equal(vector 0.0 0.0 -1.0, comps.eyev)
        Assert.Equal(vector 0.0 0.0 -1.0, comps.normalv)
        Assert.True(comps.inside)

    [<Fact>]
    let ``The hit should offset the point``() =
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = ShapeSphere.build |> Shapes.transform (translation 0.0 0.0 1.0)
        let i = Shapes.build_intersection 5.0 s []
        let comps = prepare i r [i]
        Assert.True(comps.over_point.z < -epsilon/2.0)
        Assert.True(comps.point.z > comps.over_point.z)

    [<Fact>]
    let ``Precomputing the reflection vector``() =
        let p = ShapePlane.build
        let r = {
            origin = point 0.0 1.0 -1.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = Shapes.build_intersection (Math.Sqrt(2.0)) p []
        let comps = prepare i r [i]
        Assert.Equal(vector 0.0 (Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0), comps.reflectv)

    [<Theory>]
    [<InlineData(0, 1.0, 1.5)>]
    [<InlineData(1, 1.5, 2.0)>]
    [<InlineData(2, 2.0, 2.5)>]
    [<InlineData(3, 2.5, 2.5)>]
    [<InlineData(4, 2.5, 1.5)>]
    [<InlineData(5, 1.5, 1.0)>]
    let ``Finding n1 and n2 at various intersections``(index, n1, n2) =
        let a = ShapeSphere.build |> Shapes.texture glass |> Shapes.transform (scaling 2.0 2.0 2.0)
        let b = ShapeSphere.build |> Shapes.texture { glass with refractive_index = 2.0; } |> Shapes.transform (translation 0.0 0.0 -0.25)
        let c = ShapeSphere.build |> Shapes.texture { glass with refractive_index = 2.5; } |> Shapes.transform (translation 0.0 0.0 0.25)
        let r = {
            origin = point 0.0 0.0 -4.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = [
            Shapes.build_intersection 2.0 a [];
            Shapes.build_intersection 2.75 b [];
            Shapes.build_intersection 3.25 c [];
            Shapes.build_intersection 4.75 b [];
            Shapes.build_intersection 5.25 c [];
            Shapes.build_intersection 6.0 a [];
        ]
        let comps = prepare (Seq.item(index) xs) r xs
        Assert.Equal(n1, comps.n1)
        Assert.Equal(n2, comps.n2)

    [<Fact>]
    let ``The under point is offset below the surface``() =
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = ShapeSphere.build |> Shapes.texture glass |> Shapes.transform (translation 0.0 0.0 1.0)
        let i = Shapes.build_intersection 5.0 s []
        let xs = [i]
        let comps = prepare i r xs
        Assert.True(comps.under_point.z > epsilon/2.0)
        Assert.True(comps.point.z < comps.under_point.z)

    [<Fact>]
    let ``The Schlick approximation under total internal reflection``() =
        let s = ShapeSphere.build |> Shapes.texture glass
        let r = {
            origin = point 0.0 0.0 (Math.Sqrt(2.0)/2.0);
            direction = vector 0.0 1.0 0.0;
        }
        let xs = [
            Shapes.build_intersection (-Math.Sqrt(2.0)/2.0) s [];
            Shapes.build_intersection (Math.Sqrt(2.0)/2.0) s [];
        ]
        let comps = prepare (Seq.item(1) xs) r xs
        let reflectance = schlick comps
        Assert.Equal(1.0, reflectance)

    [<Fact>]
    let ``The Schlick approximation with a perpendicular viewing angle``() =
        let s = ShapeSphere.build |> Shapes.texture glass
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = [
            Shapes.build_intersection -1.0 s [];
            Shapes.build_intersection 1.0 s [];
        ]
        let comps = prepare (Seq.item(1) xs) r xs
        let reflectance = schlick comps
        Assert.True(areEqualFloat reflectance 0.04)

    [<Fact>]
    let ``The Schlick approximation with small angle and n2 > n2``() =
        let s = ShapeSphere.build |> Shapes.texture glass
        let r = {
            origin = point 0.0 0.99 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = [
            Shapes.build_intersection 1.8589 s [];
        ]
        let comps = prepare (Seq.item(0) xs) r xs
        let reflectance = schlick comps
        Assert.Equal(0.48873081012212183, reflectance)

    [<Fact>]
    let ``An intersection can encapsulate u and v``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)
        let i = Shapes.build_intersection_triangle 3.5 t 0.2 0.4 []
        Assert.Equal(0.2, i.u)
        Assert.Equal(0.4, i.v)