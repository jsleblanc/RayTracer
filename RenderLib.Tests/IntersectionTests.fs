﻿namespace RenderLib.Tests

open System
open Xunit
open FsCheck
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Ray
open RenderLib.Shapes
open RenderLib.Translations

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

    [<Fact>]
    let ``Precomputing the state of an intersection``() =
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Sphere(shapeProperties.Default)
        let i = {
            t = 4.0;
            obj = s;
        } 
        let comps = prepare_computations i r
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
        let s = Sphere(shapeProperties.Default)
        let i = {
            t = 4.0;
            obj = s;
        } 
        let comps = prepare_computations i r
        Assert.False(comps.inside)

    [<Fact>]
    let ``The hit, when an intersection occurs on the inside``() =
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Sphere(shapeProperties.Default)
        let i = {
            t = 1.0;
            obj = s;
        } 
        let comps = prepare_computations i r
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
        let s = Sphere({shapeProperties.Default with default_transformation = translation 0.0 0.0 1.0; })
        let i = {
            t = 5.0;
            obj = s;
        } 
        let comps = prepare_computations i r
        Assert.True(comps.over_point.z < -epsilon/2.0)
        Assert.True(comps.point.z > comps.over_point.z)

    [<Fact>]
    let ``Precomputing the reflection vector``() =
        let p = Plane(shapeProperties.Default)
        let r = {
            origin = point 0.0 1.0 -1.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = {
            t = Math.Sqrt(2.0);
            obj = p;
        }
        let comps = prepare_computations i r
        Assert.Equal(vector 0.0 (Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0), comps.reflectv)