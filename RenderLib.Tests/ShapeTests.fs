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
        let s = sphere()
        Assert.Equal(identity_matrix(), s.default_transformation)

    [<Fact>]
    let ``Changing a sphere's transformation``() =
        let s = sphere()
        let t = translation 2.0 3.0 4.0
        s.default_transformation <- t
        Assert.Equal(t, s.default_transformation)

    [<Fact>]
    let ``Intersecting a scaled sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = sphere()
        s.default_transformation <- scaling 2.0 2.0 2.0
        let xs = intersect s r
        Assert.Equal(2, Seq.length xs)
        Assert.Equal(3.0, (Seq.item 0 xs).t)
        Assert.Equal(7.0, (Seq.item 1 xs).t)

    [<Fact>]
    let ``Intersecting a translated sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = sphere()
        s.default_transformation <- translation 5.0 0.0 0.0
        let xs = intersect s r
        Assert.True(Seq.isEmpty xs)

    module SphereTests = 
        
        [<Fact>]
        let ``The normal on a sphere at a point on the x axis``() =
            let s = sphere()
            let n = normal_at s (point 1.0 0.0 0.0)
            Assert.Equal(vector 1.0 0.0 0.0, n)

        [<Fact>]
        let ``The normal on a sphere at a point on the y axis``() =
            let s = sphere()
            let n = normal_at s (point 0.0 1.0 0.0)
            Assert.Equal(vector 0.0 1.0 0.0, n)

        [<Fact>]
        let ``The normal on a sphere at a point on the z axis``() =
            let s = sphere()
            let n = normal_at s (point 0.0 0.0 1.0)
            Assert.Equal(vector 0.0 0.0 1.0, n)

        [<Fact>]
        let ``The normal on a sphere at a nonaxial point``() =
            let v = Math.Sqrt(3.0) / 3.0
            let s = sphere()
            let n = normal_at s (point v v v)
            Assert.Equal(vector v v v, n)

        [<Fact>]
        let ``The normal is a normalized vector``() =
            let s = sphere()
            let v = Math.Sqrt(3.0) / 3.0
            let n = normal_at s (point v v v)
            Assert.Equal(n.normalize(), n)

        [<Fact>]
        let ``Computing the normal on a translated sphere``() =
            let s = sphere()
            s.default_transformation <- translation 0.0 1.0 0.0
            let n = normal_at s (point 0.0 1.70711 -0.70711)
            Assert.Equal(vector 0.0 0.70711 -0.70711, n)

        [<Fact>]
        let ``Computing the normal on a transformed sphere``() =
            let s = sphere()
            let m = (scaling 1.0 0.5 1.0) * rotation_z (Math.PI / 5.0)
            s.default_transformation <- m
            let v = Math.Sqrt(2.0)/2.0
            let n = normal_at s (point 0.0 v -v)
            Assert.Equal(vector 0.0 0.97014 -0.24254, n)