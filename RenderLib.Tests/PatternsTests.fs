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
open RenderLib.Lights
open RenderLib.Color

module PatternsTests = 

    [<Fact>]
    let ``Creating a stripe pattern``() =
        let p = stripe_pattern white black
        Assert.Equal(white, p.a)
        Assert.Equal(black, p.b)

    [<Fact>]
    let ``A stripe pattern is constant in y``() =
        let p = stripe_pattern white black
        Assert.Equal(white, stripe_at p (point 0.0 0.0 0.0))
        Assert.Equal(white, stripe_at p (point 0.0 1.0 0.0))
        Assert.Equal(white, stripe_at p (point 0.0 2.0 0.0))

    [<Fact>]
    let ``A stripe pattern is constant in z``() =
        let p = stripe_pattern white black
        Assert.Equal(white, stripe_at p (point 0.0 0.0 0.0))
        Assert.Equal(white, stripe_at p (point 0.0 0.0 1.0))
        Assert.Equal(white, stripe_at p (point 0.0 0.0 2.0))

    [<Fact>]
    let ``A stripe pattern alternates in x``() =
        let p = stripe_pattern white black
        Assert.Equal(white, stripe_at p (point 0.0 0.0 0.0))
        Assert.Equal(white, stripe_at p (point 0.9 0.0 0.0))
        Assert.Equal(black, stripe_at p (point 1.0 0.0 0.0))
        Assert.Equal(black, stripe_at p (point -0.1 0.0 0.0))
        Assert.Equal(black, stripe_at p (point -1.0 0.0 0.0))
        Assert.Equal(white, stripe_at p (point -1.1 0.0 0.0))