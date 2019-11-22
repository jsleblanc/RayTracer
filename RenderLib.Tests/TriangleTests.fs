namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Shapes
open RenderLib.Ray

module TriangleTests =

    [<Fact>]
    let ``Constructing a triangle`` () =
        let p1 = point 0.0 1.0 0.0
        let p2 = point -1.0 0.0 0.0
        let p3 = point 1.0 0.0 0.0
        let t = ShapeTriangle.build p1 p2 p3
        let data = ShapeTriangle.data t
        Assert.Equal(p1, data.p1)
        Assert.Equal(p2, data.p2)
        Assert.Equal(p3, data.p3)
        Assert.Equal(vector -1.0 -1.0 0.0, data.e1)
        Assert.Equal(vector 1.0 -1.0 0.0, data.e2)
        Assert.Equal(vector 0.0 0.0 -1.0, data.normal)

    [<Fact>]
    let ``Finding the normal on a triangle``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)
        let n1 = t.local_normal_at None t (point 0.0 0.5 0.0)
        let n2 = t.local_normal_at None t (point -0.5 0.75 0.0)
        let n3 = t.local_normal_at None t (point 0.5 0.25 0.0)
        let data = ShapeTriangle.data t
        Assert.Equal(n1, data.normal)
        Assert.Equal(n2, data.normal)
        Assert.Equal(n3, data.normal)

    [<Fact>]
    let ``Intersecting a ray parallel to the triangle``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)
        let r = {
            origin = point 0.0 -1.0 -2.0;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray misses the p1-p3 edge``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)
        let r = {
            origin = point 1.0 1.0 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray misses the p1-p2 edge``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)
        let r = {
            origin = point -1.0 1.0 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray misses the p2-p3 edge``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)
        let r = {
            origin = point 0.0 -1.0 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray strikes a triangle``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0)
        let r = {
            origin = point 0.0 0.5 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.Equal(1, List.length xs)

    [<Fact>]
    let ``A triangle has a bounding box``() =
        let t = ShapeTriangle.build (point -3.0 7.0 2.0) (point 6.0 2.0 -4.0) (point 2.0 -1.0 -1.0)
        Assert.Equal(point -3.0 -1.0 -4.0, t.bounding_box.minimum)
        Assert.Equal(point 6.0 7.0 2.0, t.bounding_box.maximum)
   
    let smooth_triangle () =
        let p1 = point 0.0 1.0 0.0
        let p2 = point -1.0 0.0 0.0
        let p3 = point 1.0 0.0 0.0
        let n1 = vector 0.0 1.0 0.0
        let n2 = vector -1.0 0.0 0.0
        let n3 = vector 1.0 0.0 0.0
        ShapeTriangle.build_smooth p1 p2 p3 n1 n2 n3

    [<Fact>]
    let ``Constructing a smooth triangle``() =
        let t = smooth_triangle ()
        let data = ShapeTriangle.data t
        Assert.True(data.smooth)

    [<Fact>]
    let ``An intersection with a smooth triangle stores u/v``() =
        let t = smooth_triangle ()
        let r = {
            origin = point -0.2 0.3 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.False(List.isEmpty xs)
        let i = List.item(0) xs
        Assert.True(areEqualFloat 0.45 i.u)
        Assert.True(areEqualFloat 0.25 i.v)

    [<Fact>]
    let ``A smooth triangle uses u/v to interpolate the normal``() =
        let t = smooth_triangle ()
        let i = Shapes.build_intersection_triangle 1.0 t 0.45 0.25 []
        let n = Shapes.normal_at (Some i) t [] (point 0.0 0.0 0.0)
        Assert.Equal(vector -0.5547001962 0.8320502943 0.0, n)

    [<Fact>]
    let ``Preparing the normal on a smooth triangle``() =
        let t = smooth_triangle ()
        let i = Shapes.build_intersection_triangle 1.0 t 0.45 0.25 []
        let r = {
            origin = point -0.2 0.3 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = [i]
        let comps = PreparedComputations.prepare i r xs
        Assert.Equal(vector -0.5547001962 0.8320502943 0.0, comps.normalv)