﻿namespace RenderLib.Tests

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
        let t = ShapeTriangle.build p1 p2 p3 false
        let data = ShapeTriangle.data t
        Assert.Equal(p1, data.p1)
        Assert.Equal(p2, data.p2)
        Assert.Equal(p3, data.p3)
        Assert.Equal(vector -1.0 -1.0 0.0, data.e1)
        Assert.Equal(vector 1.0 -1.0 0.0, data.e2)
        Assert.Equal(vector 0.0 0.0 -1.0, data.normal)

    [<Fact>]
    let ``Finding the normal on a triangle``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0) false
        let n1 = t.local_normal_at None t (point 0.0 0.5 0.0)
        let n2 = t.local_normal_at None t (point -0.5 0.75 0.0)
        let n3 = t.local_normal_at None t (point 0.5 0.25 0.0)
        let data = ShapeTriangle.data t
        Assert.Equal(n1, data.normal)
        Assert.Equal(n2, data.normal)
        Assert.Equal(n3, data.normal)

    [<Fact>]
    let ``Intersecting a ray parallel to the triangle``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0) false
        let r = {
            origin = point 0.0 -1.0 -2.0;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray misses the p1-p3 edge``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0) false
        let r = {
            origin = point 1.0 1.0 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray misses the p1-p2 edge``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0) false
        let r = {
            origin = point -1.0 1.0 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray misses the p2-p3 edge``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0) false
        let r = {
            origin = point 0.0 -1.0 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray strikes a triangle``() =
        let t = ShapeTriangle.build (point 0.0 1.0 0.0) (point -1.0 0.0 0.0) (point 1.0 0.0 0.0) false
        let r = {
            origin = point 0.0 0.5 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = t.local_intersect t [] r
        Assert.Equal(1, List.length xs)

    [<Fact>]
    let ``A triangle has a bounding box``() =
        let t = ShapeTriangle.build (point -3.0 7.0 2.0) (point 6.0 2.0 -4.0) (point 2.0 -1.0 -1.0) false
        Assert.Equal(point -3.0 -1.0 -4.0, t.bounding_box.minimum)
        Assert.Equal(point 6.0 7.0 2.0, t.bounding_box.maximum)