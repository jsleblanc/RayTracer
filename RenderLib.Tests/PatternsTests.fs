namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Color
open RenderLib.Tuple
open RenderLib.Translations
open RenderLib.Patterns

module PatternsTests = 

    let white_pattern () = Patterns.Solid white
    let black_pattern () = Patterns.Solid black

    [<Fact>]
    let ``A stripe pattern is constant in y``() =
        let p = Patterns.stripe (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point 0.0 1.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point 0.0 2.0 0.0))

    [<Fact>]
    let ``A stripe pattern is constant in z``() =
        let p = Patterns.stripe (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 1.0))
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 2.0))

    [<Fact>]
    let ``A stripe pattern alternates in x``() =
        let p = Patterns.stripe (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point 0.9 0.0 0.0))
        Assert.Equal(black_pattern(), p.fn (point 1.0 0.0 0.0))
        Assert.Equal(black_pattern(), p.fn (point -0.1 0.0 0.0))
        Assert.Equal(black_pattern(), p.fn (point -1.0 0.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point -1.1 0.0 0.0))

    [<Fact>]
    let ``Stripes with an object transformation``() =
        let obj = ShapeSphere.build |> Shapes.transform (scaling 2.0 2.0 2.0)
        let pattern = Patterns.stripe (solid_c white) (solid_c black)
        let c = Patterns.at_object pattern (Shapes.world_to_object obj []) (point 1.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``Stripes with a pattern transformation``() =
        let obj = ShapeSphere.build
        let pattern = Patterns.stripe (solid_c white) (solid_c black) |> Patterns.transform (scaling 2.0 2.0 2.0)
        let c = Patterns.at_object pattern (Shapes.world_to_object obj []) (point 1.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``Stripes with both an object and a pattern transformation``() =
        let obj = ShapeSphere.build
        let pattern = Patterns.stripe (solid_c white) (solid_c black) |> Patterns.transform (translation 0.5 0.0 0.0)
        let c = Patterns.at_object pattern (Shapes.world_to_object obj []) (point 2.5 0.0 0.0)
        Assert.Equal(white, c)

    [<Fact>]
    let ``A gradient linearly interpolates between colors``() =
        let p = Patterns.gradient (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(Patterns.solid 0.75 0.75 0.75, p.fn (point 0.25 0.0 0.0))
        Assert.Equal(Patterns.solid 0.5 0.5 0.5, p.fn (point 0.50 0.0 0.0))
        Assert.Equal(Patterns.solid 0.25 0.25 0.25, p.fn (point 0.75 0.0 0.0))
        Assert.Equal(Patterns.solid 0.1 0.1 0.1, p.fn (point 0.9 0.0 0.0))

    [<Fact>]
    let ``A ring should extend in both x and z``() =
        let p = Patterns.ring (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(black_pattern(), p.fn (point 1.0 0.0 0.0))
        Assert.Equal(black_pattern(), p.fn (point 0.0 0.0 1.0))
        Assert.Equal(black_pattern(), p.fn (point 0.708 0.0 0.708))

    [<Fact>]
    let ``Checkers should repeat in x``() =
        let p = Patterns.checkers (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point 0.99 0.0 0.0))
        Assert.Equal(black_pattern(), p.fn (point 1.01 0.0 0.0))

    [<Fact>]
    let ``Checkers should repeat in y``() =
        let p = Patterns.checkers (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.99 0.0))
        Assert.Equal(black_pattern(), p.fn (point 0.0 1.01 0.0))

    [<Fact>]
    let ``Checkers should repeat in z``() =
        let p = Patterns.checkers (solid_c white) (solid_c black)
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.0))
        Assert.Equal(white_pattern(), p.fn (point 0.0 0.0 0.99))
        Assert.Equal(black_pattern(), p.fn (point 0.0 0.0 1.01))
