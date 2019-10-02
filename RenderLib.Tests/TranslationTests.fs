namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Translations

module TranslationTests =

    [<Fact>]
    let ``Multiplying by a transformation matrix``() =
        let t = translation 5.0 -3.0 2.0
        let p = point -3.0 4.0 5.0
        let r = t * p
        let e = point 2.0 1.0 7.0
        Assert.Equal(e, r)

    [<Fact>]
    let ``Multiplying by the inverse of a transformation matrix``() =
        let t = translation 5.0 -3.0 2.0
        let ir = inverse t
        let p = point -3.0 4.0 5.0
        let e = point -8.0 7.0 3.0
        match ir with
        | Ok i -> Assert.Equal(e, i * p)
        | Error s -> Assert.True(false, s)

    [<Fact>]
    let ``Translation does not affect vectors``() =
        let t = translation 5.0 -3.0 2.0
        let v = vector -3.0 4.0 5.0
        let r = t * v
        Assert.Equal(v, r)

    [<Fact>]
    let ``A scaling matrix applied to a point``() =
        let t = scaling 2.0 3.0 4.0
        let p = point -4.0 6.0 8.0
        let r = t * p
        let e = point -8.0 18.0 32.0
        Assert.Equal(e, r)

    [<Fact>]
    let ``A scaling matrix applied to a vector`` () =
        let t = scaling 2.0 3.0 4.0
        let v = vector -4.0 6.0 8.0
        let r = t * v
        let e = vector -8.0 18.0 32.0
        Assert.Equal(e, r)

    [<Fact>]
    let ``Multiplying by the inverse of a scaling matrix``() =
        let t = scaling 2.0 3.0 4.0
        let v = vector -4.0 6.0 8.0
        let e = vector -2.0 2.0 2.0
        let invTr = inverse t
        match invTr with
        | Ok invT -> Assert.Equal(e, invT * v)
        | Error s -> Assert.True(false, s)

    [<Fact>]
    let ``Reflection is scaling by a negative value``() =
        let t = scaling -1.0 1.0 1.0
        let p = point 2.0 3.0 4.0
        let e = point -2.0 3.0 4.0
        let r = t * p
        Assert.Equal(e, r)

    [<Fact>]
    let ``Rotating a point around the x axis``() =
        let p = point 0.0 1.0 0.0
        let half_quarter = rotation_x (Math.PI / 4.0)
        let full_quarter = rotation_x (Math.PI / 2.0)
        let v = (Math.Sqrt 2.0) / 2.0
        Assert.Equal(point 0.0 v v, half_quarter * p)
        Assert.Equal(point 0.0 0.0 1.0, full_quarter * p)

    [<Fact>]
    let ``The inverse of an x-rotation rotates in the opposite direction``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 1.0 0.0
        let half_quarter = rotation_x (Math.PI / 4.0)
        let inv = inverse half_quarter
        match inv with
        | Ok i -> Assert.Equal(point 0.0 v -v, i * p)
        | Error s -> Assert.True(false, s)

    [<Fact>]
    let ``Rotating a point around the y axix``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 0.0 1.0
        let half_quarter = rotation_y (Math.PI / 4.0)
        let full_quarter = rotation_y (Math.PI / 2.0)
        Assert.Equal(point v 0.0 v, half_quarter * p)
        Assert.Equal(point 1.0 0.0 0.0, full_quarter * p)

    [<Fact>]
    let ``The inverse of an y-rotation rotates in the opposite direction``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 0.0 1.0
        let half_quarter = rotation_y (Math.PI / 4.0)
        let inv = inverse half_quarter
        match inv with
        | Ok i -> Assert.Equal(point -v 0.0 v, i * p)
        | Error s -> Assert.True(false, s)

    [<Fact>]
    let ``Rotating a point around the z axix``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 0.0 1.0 0.0
        let half_quarter = rotation_z (Math.PI / 4.0)
        let full_quarter = rotation_z (Math.PI / 2.0)
        Assert.Equal(point -v v 0.0, half_quarter * p)
        Assert.Equal(point -1.0 0.0 0.0, full_quarter * p)

    [<Fact>]
    let ``The inverse of an z-rotation rotates in the opposite direction``() =
        let v = (Math.Sqrt 2.0) / 2.0
        let p = point 1.0 0.0 0.0
        let half_quarter = rotation_z (Math.PI / 4.0)
        let inv = inverse half_quarter
        match inv with
        | Ok i -> Assert.Equal(point v -v 0.0, i * p)
        | Error s -> Assert.True(false, s)

