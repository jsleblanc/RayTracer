namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Ray

module ShapeTests = 
    
    [<Fact>]
    let ``A sphere's default transformation``() =
        let s = Sphere(shapeProperties.Default)
        match s with
        | Sphere sp -> Assert.Equal(identity_matrix(), sp.default_transformation)

    [<Fact>]
    let ``Changing a sphere's transformation``() =
        let s = Sphere(shapeProperties.Default)
        let t = translation 2.0 3.0 4.0
        match s with
        | Sphere sp ->
            let x = { sp with default_transformation = t; }
            Assert.Equal(t, x.default_transformation)

    [<Fact>]
    let ``Intersecting a scaled sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = Sphere(shapeProperties.Default)
        match s with
        | Sphere sp ->
            let x = Sphere { sp with default_transformation = scaling 2.0 2.0 2.0; }
            let xs = intersect x r
            Assert.Equal(2, Seq.length xs)
            Assert.Equal(3.0, (Seq.item 0 xs).t)
            Assert.Equal(7.0, (Seq.item 1 xs).t)

    [<Fact>]
    let ``Intersecting a translated sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = Sphere(shapeProperties.Default)
        match s with
        | Sphere sp ->
            let x = Sphere { sp with default_transformation = translation 5.0 0.0 0.0; }
            let xs = intersect x r
            Assert.True(Seq.isEmpty xs)

    module SphereTests = 
        
        [<Fact>]
        let ``The normal on a sphere at a point on the x axis``() =
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point 1.0 0.0 0.0)
            Assert.Equal(vector 1.0 0.0 0.0, n)

        [<Fact>]
        let ``The normal on a sphere at a point on the y axis``() =
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point 0.0 1.0 0.0)
            Assert.Equal(vector 0.0 1.0 0.0, n)

        [<Fact>]
        let ``The normal on a sphere at a point on the z axis``() =
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point 0.0 0.0 1.0)
            Assert.Equal(vector 0.0 0.0 1.0, n)

        [<Fact>]
        let ``The normal on a sphere at a nonaxial point``() =
            let v = Math.Sqrt(3.0) / 3.0
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point v v v)
            Assert.Equal(vector v v v, n)

        [<Fact>]
        let ``The normal is a normalized vector``() =
            let s = Sphere(shapeProperties.Default)
            let v = Math.Sqrt(3.0) / 3.0
            let n = normal_at s (point v v v)
            Assert.Equal(n.normalize(), n)

        [<Fact>]
        let ``Computing the normal on a translated sphere``() =
            let s = Sphere(shapeProperties.Default)
            match s with
            | Sphere sp ->
                let x = Sphere { sp with default_transformation = translation 0.0 1.0 0.0; }                
                let n = normal_at x (point 0.0 1.70711 -0.70711)
                Assert.Equal(vector 0.0 0.7071067812 -0.7071067812, roundtuple n)

        [<Fact>]
        let ``Computing the normal on a transformed sphere``() =
            let s = Sphere(shapeProperties.Default)
            match s with
            | Sphere sp ->
                let x = Sphere { sp with default_transformation = (scaling 1.0 0.5 1.0) * rotation_z (Math.PI / 5.0); }
                let n = normal_at x (point 0.0 (Math.Sqrt(2.0)/2.0) (-Math.Sqrt(2.0)/2.0))
                Assert.Equal(vector 0.0 0.9701425001 -0.242535625, roundtuple n)