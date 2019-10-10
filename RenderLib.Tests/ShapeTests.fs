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
        let s = Sphere (sphere())
        s.default_transformation <- scaling 2.0 2.0 2.0
        let xs = intersect s r
        Assert.Equal(2, Seq.length xs)
