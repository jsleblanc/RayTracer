namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Color
open RenderLib.Tuple
open RenderLib.Shapes
open RenderLib.Ray
open RenderLib.Translations

module CSGTests =

    [<Fact>]
    let ``CSG is created with an operation and two shapes``() =
        let s1 = ShapeSphere.build
        let s2 = ShapeCube.build
        let c = ShapeCSG.union s1 s2
        match c.shape with
        | Union (left,right,_) ->
            Assert.Equal(s1, left)
            Assert.Equal(s2, right)
        | _ -> Assert.True(false, "expected a Union of s1,s2")

    [<Theory>]
    [<InlineData(true,true,true,false)>]
    [<InlineData(true,true,false,true)>]
    [<InlineData(true,false,true,false)>]
    [<InlineData(true,false,false,true)>]
    [<InlineData(false,true,true,false)>]
    [<InlineData(false,true,false,false)>]
    [<InlineData(false,false,true,true)>]
    [<InlineData(false,false,false,true)>]
    let ``Evaluating the rule for a CSG union operation``(lhit,inl,inr,result) =
        let s1 = ShapeSphere.build
        let s2 = ShapeCube.build
        let c = ShapeCSG.union s1 s2
        match c.shape with
        | Union (_,_,fn) ->
            let actual = fn lhit inl inr
            Assert.Equal(result, actual)
        | _ -> Assert.True(false, "expected a Union of s1,s2")

    [<Theory>]
    [<InlineData(true,true,true,true)>]
    [<InlineData(true,true,false,false)>]
    [<InlineData(true,false,true,true)>]
    [<InlineData(true,false,false,false)>]
    [<InlineData(false,true,true,true)>]
    [<InlineData(false,true,false,true)>]
    [<InlineData(false,false,true,false)>]
    [<InlineData(false,false,false,false)>]
    let ``Evaluating the rule for a CSG intersect operation``(lhit,inl,inr,result) =
        let s1 = ShapeSphere.build
        let s2 = ShapeCube.build
        let c = ShapeCSG.intersect s1 s2
        match c.shape with
        | Intersect (_,_,fn) ->
            let actual = fn lhit inl inr
            Assert.Equal(result, actual)
        | _ -> Assert.True(false, "expected a Intersect of s1,s2")

    [<Theory>]
    [<InlineData(true,true,true,false)>]
    [<InlineData(true,true,false,true)>]
    [<InlineData(true,false,true,false)>]
    [<InlineData(true,false,false,true)>]
    [<InlineData(false,true,true,true)>]
    [<InlineData(false,true,false,true)>]
    [<InlineData(false,false,true,false)>]
    [<InlineData(false,false,false,false)>]
    let ``Evaluating the rule for a CSG difference operation``(lhit,inl,inr,result) =
        let s1 = ShapeSphere.build
        let s2 = ShapeCube.build
        let c = ShapeCSG.difference s1 s2
        match c.shape with
        | Difference (_,_,fn) ->
            let actual = fn lhit inl inr
            Assert.Equal(result, actual)
        | _ -> Assert.True(false, "expected a Difference of s1,s2")

    let filtering_intersections_tests operation x0 x1 =
        let s1 = ShapeSphere.build
        let s2 = ShapeCube.build
        let c = operation s1 s2
        let xs = [
            Shapes.build_intersection 1.0 s1 [];
            Shapes.build_intersection 2.0 s2 [];
            Shapes.build_intersection 3.0 s1 [];
            Shapes.build_intersection 4.0 s2 [];
        ]
        let result = ShapeCSG.filter_intersections c xs
        Assert.Equal(2, List.length result)
        Assert.Equal(List.item(0) result, List.item(x0) xs)
        Assert.Equal(List.item(1) result, List.item(x1) xs)

    [<Fact>]
    let ``Filtering a list of intersections for a union operation``() =
        filtering_intersections_tests ShapeCSG.union 0 3

    [<Fact>]
    let ``Filtering a list of intersections for a intersect operation``() =
        filtering_intersections_tests ShapeCSG.intersect 1 2
    
    [<Fact>]
    let ``Filtering a list of intersections for a difference operation``() =
        filtering_intersections_tests ShapeCSG.difference 0 1

    [<Fact>]
    let ``A ray misses a CSG object``() =
        let s1 = ShapeSphere.build
        let s2 = ShapeCube.build
        let c = ShapeCSG.union s1 s2
        let r = {
            origin = point 0.0 2.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = c.local_intersect c [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``A ray hits a CSG objectg``() =
        let s1 = ShapeSphere.build
        let s2 = ShapeSphere.build |> Shapes.transform (translation 0.0 0.0 0.5)
        let c = ShapeCSG.union s1 s2
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = c.local_intersect c [] r
        Assert.Equal(2, List.length xs)
        let i1 = List.item(0) xs
        Assert.Equal(4.0, i1.t)
        Assert.Equal(s1, i1.obj)
        let i2 = List.item(1) xs
        Assert.Equal(6.5, i2.t)
        Assert.Equal(s2, i2.obj)

    [<Fact>]
    let ``Intersecting ray+csg doesn't test children if box is missed``() =
        let left = ShapeTests.test_shape()
        let right = ShapeTests.test_shape()
        let shape = ShapeCSG.difference left right
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = Shapes.intersect shape [] r
        Assert.True((ShapeTests.test_shape_data left).ray.IsNone)
        Assert.True((ShapeTests.test_shape_data right).ray.IsNone)

    [<Fact>]
    let ``Intersecting ray+csg tests children if box is hit``() =
        let left = ShapeTests.test_shape()
        let right = ShapeTests.test_shape()
        let shape = ShapeCSG.difference left right
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = Shapes.intersect shape [] r
        Assert.True((ShapeTests.test_shape_data left).ray.IsSome)
        Assert.True((ShapeTests.test_shape_data right).ray.IsSome)

    [<Fact>]
    let ``Subdividing a CSG shape subdivides its children``() =
        let s1 = ShapeSphere.build |> Shapes.transform (translation -1.5 0.0 0.0)
        let s2 = ShapeSphere.build |> Shapes.transform (translation 1.5 0.0 0.0)
        let left = ShapeGroup.build [s1;s2;]
        let s3 = ShapeSphere.build |> Shapes.transform (translation 0.0 0.0 -1.5)
        let s4 = ShapeSphere.build |> Shapes.transform (translation 0.0 0.0 1.5)
        let right = ShapeGroup.build [s3;s4;]
        let shape = ShapeCSG.difference left right
        let shape_d = ShapeGroup.divide shape
        Assert.True(false)