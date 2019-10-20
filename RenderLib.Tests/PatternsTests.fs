﻿namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Color
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Translations
open RenderLib.Ray
open RenderLib.Material
open RenderLib.Patterns
open RenderLib.Shapes

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

    [<Fact>]
    let ``Stripes with an object transformation``() =
        let obj = Sphere({ shapeProperties.Default with default_transformation = scaling 2.0 2.0 2.0; })
        let pattern = stripe_pattern white black
        let c = stripe_at_object pattern obj (point 1.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``Stripes with a pattern transformation``() =
        let obj = Sphere({ shapeProperties.Default with pattern_transformation = scaling 2.0 2.0 2.0; })
        let pattern = stripe_pattern white black
        let c = stripe_at_object pattern obj (point 1.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``Stripes with both an object and a pattern transformation``() =
        let obj = Sphere({ shapeProperties.Default with default_transformation = scaling 2.0 2.0 2.0; pattern_transformation = translation 0.5 0.0 0.0; })
        let pattern = stripe_pattern white black
        let c = stripe_at_object pattern obj (point 2.5 0.0 0.0)
        Assert.Equal(white, c)