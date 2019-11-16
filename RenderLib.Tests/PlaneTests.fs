namespace RenderLib.Tests

open Xunit
open RenderLib.Tuple
open RenderLib.Shapes2
open RenderLib.Ray

module PlaneTests =

    [<Fact>]
    let ``The normal of a plane is a constant everywhere``() =
        let p = RenderLib.ShapePlane.build
        let n1 = normal_at None p [] (point 0.0 0.0 0.0)
        let n2 = normal_at None p [] (point 10.0 0.0 -10.0)
        let n3 = normal_at None p [] (point -5.0 0.0 150.0)
        let v = vector 0.0 1.0 0.0
        Assert.Equal(v, n1)
        Assert.Equal(v, n2)
        Assert.Equal(v, n3)
    
    [<Fact>]
    let ``Intersect with a ray parallel to the plane``() =
        let p = RenderLib.ShapePlane.build
        let r = {
            origin = point 0.0 10.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = intersect p [] r
        Assert.True(Seq.isEmpty xs)

    [<Fact>]
    let ``Intersect with a coplanar ray``() =
        let p = RenderLib.ShapePlane.build
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = intersect p [] r
        Assert.True(Seq.isEmpty xs)

    [<Fact>]
    let ``A ray intersecting a plane from above``() =
        let p = RenderLib.ShapePlane.build
        let r = {
            origin = point 0.0 1.0 0.0;
            direction = vector 0.0 -1.0 0.0;
        }
        let xs = intersect p [] r
        Assert.Equal(1, Seq.length xs)
        Assert.Equal(1.0, (Seq.item 0 xs).t)
        Assert.Equal(p, (Seq.item 0 xs).obj)

    [<Fact>]
    let ``A ray intersecting a plane from below``() =
        let p = RenderLib.ShapePlane.build
        let r = {
            origin = point 0.0 -1.0 0.0;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = intersect p [] r
        Assert.Equal(1, Seq.length xs)
        Assert.Equal(1.0, (Seq.item 0 xs).t)
        Assert.Equal(p, (Seq.item 0 xs).obj)