﻿namespace RenderLib.Tests

open System
open Xunit
open FsCheck
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Ray
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Material
open RenderLib.Matrix

module IntersectionTests = 
    
    [<Fact>]
    let ``An intersection encapsulates t and object``() =
        let s = Sphere(material.Default,identity_matrix(),None)
        let i = {
            t = 3.5;
            obj = s;
        }
        Assert.Equal(s, i.obj)

    [<Fact>]
    let ``The hit, when all intersections have positive t``() =
        let s = Sphere(material.Default,identity_matrix(),None)
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
        let s = Sphere(material.Default,identity_matrix(),None)
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
        let s = Sphere(material.Default,identity_matrix(),None)
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
        let s = Sphere(material.Default,identity_matrix(),None)
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
        let s = Sphere(material.Default,identity_matrix(),None)
        let i = {
            t = 4.0;
            obj = s;
        } 
        let comps = prepare_computations i r (seq {i})
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
        let s = Sphere(material.Default,identity_matrix(),None)
        let i = {
            t = 4.0;
            obj = s;
        } 
        let comps = prepare_computations i r (seq {i})
        Assert.False(comps.inside)

    [<Fact>]
    let ``The hit, when an intersection occurs on the inside``() =
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Sphere(material.Default,identity_matrix(),None)
        let i = {
            t = 1.0;
            obj = s;
        } 
        let comps = prepare_computations i r (seq {i})
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
        let s = Sphere(material.Default,translation 0.0 0.0 1.0,None)
        let i = {
            t = 5.0;
            obj = s;
        } 
        let comps = prepare_computations i r (seq {i})
        Assert.True(comps.over_point.z < -epsilon/2.0)
        Assert.True(comps.point.z > comps.over_point.z)

    [<Fact>]
    let ``Precomputing the reflection vector``() =
        let p = Plane(material.Default,identity_matrix(),None)
        let r = {
            origin = point 0.0 1.0 -1.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = {
            t = Math.Sqrt(2.0);
            obj = p;
        }
        let comps = prepare_computations i r (seq {i})
        Assert.Equal(vector 0.0 (Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0), comps.reflectv)

    [<Theory>]
    [<InlineData(0, 1.0, 1.5)>]
    [<InlineData(1, 1.5, 2.0)>]
    [<InlineData(2, 2.0, 2.5)>]
    [<InlineData(3, 2.5, 2.5)>]
    [<InlineData(4, 2.5, 1.5)>]
    [<InlineData(5, 1.5, 1.0)>]
    let ``Finding n1 and n2 at various intersections``(index, n1, n2) =
        let a = Sphere(glass,scaling 2.0 2.0 2.0,None)
        let b = Sphere({ glass with refractive_index = 2.0; },translation 0.0 0.0 -0.25,None)
        let c = Sphere({ glass with refractive_index = 2.5; },translation 0.0 0.0 0.25,None)
        let r = {
            origin = point 0.0 0.0 -4.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = seq {
            { t = 2.0; obj = a; };
            { t = 2.75; obj = b; };
            { t = 3.25; obj = c; };
            { t = 4.75; obj = b; };
            { t = 5.25; obj = c; };
            { t = 6.0; obj = a; };
        }
        let comps = prepare_computations (Seq.item(index) xs) r xs
        Assert.Equal(n1, comps.n1)
        Assert.Equal(n2, comps.n2)

    [<Fact>]
    let ``The under point is offset below the surface``() =
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Sphere(glass,translation 0.0 0.0 1.0,None)
        let i = {
            t = 5.0;
            obj = s;
        }
        let xs = seq {i}
        let comps = prepare_computations i r xs
        Assert.True(comps.under_point.z > epsilon/2.0)
        Assert.True(comps.point.z < comps.under_point.z)

    [<Fact>]
    let ``The Schlick approximation under total internal reflection``() =
        let s = Sphere(glass,identity_matrix(),None)
        let r = {
            origin = point 0.0 0.0 (Math.Sqrt(2.0)/2.0);
            direction = vector 0.0 1.0 0.0;
        }
        let xs = seq {
            { t = (-Math.Sqrt(2.0)/2.0); obj = s; };
            { t = (Math.Sqrt(2.0)/2.0); obj = s; };
        }
        let comps = prepare_computations (Seq.item(1) xs) r xs
        let reflectance = schlick comps
        Assert.Equal(1.0, reflectance)

    [<Fact>]
    let ``The Schlick approximation with a perpendicular viewing angle``() =
        let s = Sphere(glass,identity_matrix(),None)
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = seq {
            { t = -1.0; obj = s; };
            { t = 1.0; obj = s; };
        }
        let comps = prepare_computations (Seq.item(1) xs) r xs
        let reflectance = schlick comps
        Assert.True(areEqualFloat reflectance 0.04)

    [<Fact>]
    let ``The Schlick approximation with small angle and n2 > n2``() =
        let s = Sphere(glass,identity_matrix(),None)
        let r = {
            origin = point 0.0 0.99 -2.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = seq {
            { t = 1.8589; obj = s; };
        }
        let comps = prepare_computations (Seq.item(0) xs) r xs
        let reflectance = schlick comps
        Assert.Equal(0.48873081012212183, reflectance)
