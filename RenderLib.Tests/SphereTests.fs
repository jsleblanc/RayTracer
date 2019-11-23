namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Tuple
open RenderLib.Shapes
open RenderLib.Ray
open RenderLib.Translations

module SphereTests =

    [<Fact>]
    let ``A sphere has an id``() =
        let s = ShapeSphere.build
        Assert.False(s.id = Guid.Empty)

    [<Fact>]
    let ``The normal on a sphere at a point on the x axis``() =
        let s = ShapeSphere.build
        let n = s.local_normal_at None s (point 1.0 0.0 0.0)
        Assert.Equal(vector 1.0 0.0 0.0, n)

    [<Fact>]
    let ``The normal on a sphere at a point on the y axis``() =
        let s = ShapeSphere.build
        let n = s.local_normal_at None s (point 0.0 1.0 0.0)
        Assert.Equal(vector 0.0 1.0 0.0, n)

    [<Fact>]
    let ``The normal on a sphere at a point on the z axis``() =
        let s = ShapeSphere.build
        let n = s.local_normal_at None s (point 0.0 0.0 1.0)
        Assert.Equal(vector 0.0 0.0 1.0, n)

    [<Fact>]
    let ``The normal on a sphere at a nonaxial point``() =
        let v = Math.Sqrt(3.0) / 3.0
        let s = ShapeSphere.build
        let n = s.local_normal_at None s (point v v v)
        Assert.Equal(vector v v v, n)

    [<Fact>]
    let ``The normal is a normalized vector``() =
        let s = ShapeSphere.build
        let v = Math.Sqrt(3.0) / 3.0
        let n = s.local_normal_at None s (point v v v)
        Assert.Equal(n.normalize(), n)

    [<Fact>]
    let ``Computing the normal on a translated sphere``() =
        let s = ShapeSphere.build |> Shapes.transform (translation 0.0 1.0 0.0)
        let n = normal_at None s [] (point 0.0 1.70711 -0.70711)
        Assert.Equal(vector 0.0 0.7071067812 -0.7071067812, n)

    [<Fact>]
    let ``Computing the normal on a transformed sphere``() =
        let s = ShapeSphere.build |> Shapes.transform ((scaling 1.0 0.5 1.0) * rotation_z (Math.PI / 5.0))
        let n = normal_at None s [] (point 0.0 (Math.Sqrt(2.0)/2.0) (-Math.Sqrt(2.0)/2.0))
        Assert.Equal(vector 0.0 0.97014250014533188 -0.24253562503633294, n)

    [<Fact>]
    let ``Intersecting a scaled sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = ShapeSphere.build |> Shapes.transform (scaling 2.0 2.0 2.0)
        let xs = intersect s [] r
        Assert.Equal(2, Seq.length xs)
        Assert.Equal(3.0, (List.item 0 xs).t)
        Assert.Equal(7.0, (List.item 1 xs).t)

    [<Fact>]
    let ``Intersecting a translated sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let s = ShapeSphere.build |> Shapes.transform (translation 5.0 0.0 0.0)
        let xs = intersect s [] r
        Assert.True(List.isEmpty xs)

    [<Fact>]
    let ``Building sphere should calculate and cache bounding box``() =
        let s = ShapeSphere.build
        let expected = s.bounds_of s
        Assert.Equal(expected, s.bounding_box)

    [<Fact>]
    let ``Building sphere should calculate and cache bounding box with transformation``() =
        let s = ShapeSphere.build |> Shapes.transform (scaling 5.0 5.0 5.0)
        let expected = s.bounds_of s
        Assert.Equal(expected, s.bounding_box)