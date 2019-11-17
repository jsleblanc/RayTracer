namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Tuple
open RenderLib.Translations
open RenderLib.Shapes2
open RenderLib.Ray

module GroupTests = 

    [<Fact>]
    let ``Adding a child to a group``() =
        let s = ShapeSphere.build
        let g = ShapeGroup.build [s;]
        let children = ShapeGroup.get_children g
        Assert.True(List.contains s children)

    [<Fact>]
    let ``Intersecting a ray with an empty group``() =
        let g = ShapeGroup.build []
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = g.local_intersect g [] r
        Assert.True(Seq.isEmpty xs)

    [<Fact>]
    let ``Intersecting a ray with a nonempty group``() =
        let s1 = ShapeSphere.build
        let s2 = ShapeSphere.build |> Shapes2.transform (translation 0.0 0.0 -3.0)
        let s3 = ShapeSphere.build |> Shapes2.transform (translation 5.0 0.0 0.0)
        let g = ShapeGroup.build [s1;s2;s3;]
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = g.local_intersect g [] r
        Assert.Equal(4, Seq.length xs)
        Assert.Equal((Seq.item(0) xs).obj, s2)
        Assert.Equal((Seq.item(1) xs).obj, s2)
        Assert.Equal((Seq.item(2) xs).obj, s1)
        Assert.Equal((Seq.item(3) xs).obj, s1)

    [<Fact>]
    let ``Intersecting a transformed group``() =
        let s = ShapeSphere.build |> Shapes2.transform (translation 5.0 0.0 0.0)
        let g = ShapeGroup.build [s;] |> Shapes2.transform (scaling 2.0 2.0 2.0)
        let r = {
            origin = point 10.0 0.0 -10.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = intersect g [] r
        Assert.Equal(2, Seq.length xs)

    [<Fact>]
    let ``Converting a point from world to object space``() =        
        let s = ShapeSphere.build |> Shapes2.transform (translation 5.0 0.0 0.0)
        let g2 = ShapeGroup.build [s;] |> Shapes2.transform (scaling 2.0 2.0 2.0)        
        let g1 = ShapeGroup.build [g2;] |> Shapes2.transform (rotation_y (Math.PI/2.0))
        let p = world_to_object s [g2;g1;] (point -2.0 0.0 -10.0)
        Assert.Equal(point 0.0 0.0 -1.0, p)

    [<Fact>]
    let ``Converting a normal from object to world space``() =
        let s = ShapeSphere.build |> Shapes2.transform (translation 5.0 0.0 0.0)
        let g2 = ShapeGroup.build [s;] |> Shapes2.transform (scaling 1.0 2.0 3.0)
        let g1 = ShapeGroup.build [g2;] |> Shapes2.transform (rotation_y (Math.PI/2.0))
        let q = (Math.Sqrt(3.0)/3.0) 
        let n = normal_to_world s [g2;g1;] (vector q q q)
        Assert.Equal(vector 0.2857142857 0.4285714286 -0.8571428571, n)

    [<Fact>]
    let ``Finding the nomral on a child object``() =
        let s = ShapeSphere.build |> Shapes2.transform (translation 5.0 0.0 0.0)
        let g2 = ShapeGroup.build [s;] |> Shapes2.transform (scaling 1.0 2.0 3.0)
        let g1 = ShapeGroup.build [g2;] |> Shapes2.transform (rotation_y (Math.PI/2.0))
        let world_p = point 1.7321 1.1547 -5.5774
        let n = normal_at None s [g2;g1;] world_p
        Assert.Equal(vector 0.2857036818 0.4285431518 -0.8571605294, n)

    [<Fact>]
    let ``Converting a point from world to object space nested groups``() =
        let s = ShapeSphere.build |> Shapes2.transform ((rotation_z (Math.PI/3.0))*(scaling 1.0 0.5 0.5))
        let g2 = ShapeGroup.build [s;] |> Shapes2.transform (rotation_z (Math.PI/4.0))
        let g1 = ShapeGroup.build [g2;] |> Shapes2.transform (rotation_z (Math.PI/3.0))        
        let p = world_to_object s [g2;g1;] (point -0.4975074385 -0.2721372954 -0.2336173106)
        Assert.Equal(point 0.4101209687 0.7832576841 -0.4672346213, p)

    [<Fact>]
    let ``Converting a normal from object to world space nested groups``() =
        let s = ShapeSphere.build |> Shapes2.transform ((rotation_z (Math.PI/3.0))*(scaling 1.0 0.5 0.5))
        let g1 = ShapeGroup.build [s;] |> Shapes2.transform (rotation_z (Math.PI/3.0))
        let g2 = ShapeGroup.build [g1;] |> Shapes2.transform (rotation_z (Math.PI/4.0))
        let v = normal_to_world s [g2;g1;] (vector 0.4101209687 0.7832576841 -0.4672346213)
        Assert.Equal(vector -0.428749814 -0.7525625233 -0.4998232143, v)
