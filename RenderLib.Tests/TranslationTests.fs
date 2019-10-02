namespace RenderLib.Tests

open Xunit
open FsCheck
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