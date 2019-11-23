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

    let test_shape_data shape =
        match shape.shape with
        | TestShape (data) -> data
        | _ -> failwith "expected a TestShape in test_shape_data"

    let test_shape () =
        let local_intersect shape trail ray =
            match shape.shape with
            | TestShape data -> data.ray <- Some ray
            | _ -> failwith "shouldn't ever get here"
            []
        let normal_at (hit:intersection option) shape pt =
            vector pt.x pt.y pt.z
        let bounds_of shape = 
            BoundingBoxes.build (point -1.0 -1.0 -1.0) (point 1.0 1.0 1.0)
        Shapes.build (TestShape({ ray = None; })) local_intersect normal_at bounds_of

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
