namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Color
open RenderLib.Tuple
open RenderLib.Shapes
open RenderLib.Ray
open RenderLib.Translations

module ShapeTests = 

    [<Fact>]
    let ``Transforming a shape generates a new shape id``() =
        let s1 = ShapeSphere.build
        let s2 = s1 |> Shapes.transform (scaling 2.0 2.0 2.0)
        Assert.False(s1.id = s2.id)

    [<Fact>]
    let ``Texturing a shape generates a new shape id``() =
        let s1 = ShapeSphere.build
        let s2 = s1 |> Shapes.texture Material.material.Default
        Assert.False(s1.id = s2.id)

    [<Fact>]
    let ``Patterning a shape generates a new shape id``() =
        let s1 = ShapeSphere.build
        let s2 = s1 |> Shapes.pattern (Patterns.stripe (Patterns.solid_c white) (Patterns.solid_c black))
        Assert.False(s1.id = s2.id)
