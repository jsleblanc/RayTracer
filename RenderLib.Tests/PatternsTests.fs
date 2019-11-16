namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib
open RenderLib.Color
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Translations
open RenderLib.Ray
open RenderLib.Material
open RenderLib.Patterns
open RenderLib.Shapes2
open RenderLib.Lights

module PatternsTests = 

    [<Fact>]
    let ``Creating a stripe pattern``() =
        let p = stripe_pattern_default white black
        match p with
        | Stripe (_,a,b) ->
            Assert.Equal(white, a)
            Assert.Equal(black, b)

    [<Fact>]
    let ``A stripe pattern is constant in y``() =
        let p = stripe_pattern_default white black
        Assert.Equal(white, pattern_at p (point 0.0 0.0 0.0))
        Assert.Equal(white, pattern_at p (point 0.0 1.0 0.0))
        Assert.Equal(white, pattern_at p (point 0.0 2.0 0.0))

    [<Fact>]
    let ``A stripe pattern is constant in z``() =
        let p = stripe_pattern_default white black
        Assert.Equal(white, pattern_at p (point 0.0 0.0 0.0))
        Assert.Equal(white, pattern_at p (point 0.0 0.0 1.0))
        Assert.Equal(white, pattern_at p (point 0.0 0.0 2.0))

    [<Fact>]
    let ``A stripe pattern alternates in x``() =
        let p = stripe_pattern_default white black
        Assert.Equal(white, pattern_at p (point 0.0 0.0 0.0))
        Assert.Equal(white, pattern_at p (point 0.9 0.0 0.0))
        Assert.Equal(black, pattern_at p (point 1.0 0.0 0.0))
        Assert.Equal(black, pattern_at p (point -0.1 0.0 0.0))
        Assert.Equal(black, pattern_at p (point -1.0 0.0 0.0))
        Assert.Equal(white, pattern_at p (point -1.1 0.0 0.0))

    [<Fact>]
    let ``Stripes with an object transformation``() =
        let obj = ShapeSphere.build |> Shapes2.transform (scaling 2.0 2.0 2.0)
        let pattern = stripe_pattern_default white black
        let c = pattern_at_object pattern obj (point 1.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``Stripes with a pattern transformation``() =
        let obj = ShapeSphere.build
        let pattern = stripe_pattern (scaling 2.0 2.0 2.0) white black
        let c = pattern_at_object pattern obj (point 1.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``Stripes with both an object and a pattern transformation``() =
        let obj = ShapeSphere.build
        let pattern = stripe_pattern (translation 0.5 0.0 0.0) white black
        let c = pattern_at_object pattern obj (point 2.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``A gradient linearly interpolates between colors``() =
        let pattern = gradient_pattern_default white black
        Assert.Equal(white, pattern_at pattern (point 0.0 0.0 0.0))
        Assert.Equal(color 0.75 0.75 0.75, pattern_at pattern (point 0.25 0.0 0.0))
        Assert.Equal(color 0.5 0.5 0.5, pattern_at pattern (point 0.50 0.0 0.0))
        Assert.Equal(color 0.25 0.25 0.25, pattern_at pattern (point 0.75 0.0 0.0))
        Assert.Equal(color 0.1 0.1 0.1, pattern_at pattern (point 0.9 0.0 0.0))

    [<Fact>]
    let ``A ring should extend in both x and z``() =
        let pattern = ring_pattern_default white black
        Assert.Equal(white, pattern_at pattern (point 0.0 0.0 0.0))
        Assert.Equal(black, pattern_at pattern (point 1.0 0.0 0.0))
        Assert.Equal(black, pattern_at pattern (point 0.0 0.0 1.0))
        Assert.Equal(black, pattern_at pattern (point 0.708 0.0 0.708))

    [<Fact>]
    let ``Checkers should repeat in x``() =
        let pattern = checkers_pattern_default white black
        Assert.Equal(white, pattern_at pattern (point 0.0 0.0 0.0))
        Assert.Equal(white, pattern_at pattern (point 0.99 0.0 0.0))
        Assert.Equal(black, pattern_at pattern (point 1.01 0.0 0.0))

    [<Fact>]
    let ``Checkers should repeat in y``() =
        let pattern = checkers_pattern_default white black
        Assert.Equal(white, pattern_at pattern (point 0.0 0.0 0.0))
        Assert.Equal(white, pattern_at pattern (point 0.0 0.99 0.0))
        Assert.Equal(black, pattern_at pattern (point 0.0 1.01 0.0))

    [<Fact>]
    let ``Checkers should repeat in z``() =
        let pattern = checkers_pattern_default white black
        Assert.Equal(white, pattern_at pattern (point 0.0 0.0 0.0))
        Assert.Equal(white, pattern_at pattern (point 0.0 0.0 0.99))
        Assert.Equal(black, pattern_at pattern (point 0.0 0.0 1.01))

