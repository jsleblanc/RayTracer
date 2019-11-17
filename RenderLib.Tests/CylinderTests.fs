namespace RenderLib.Tests

open Xunit
open RenderLib
open RenderLib.Tuple
open RenderLib.Shapes2
open RenderLib.Ray

module CylinderTests =

    [<Fact>]
    let ``A ray misses a cylinder``() =
        let ray_misses o (v:tuple) =
            let cyl = ShapeCylinder.build_default
            let direction = v.normalize()
            let r = {
                origin = o;
                direction = direction;
            }
            let xs = intersect cyl [] r
            Seq.length xs = 0
        Assert.True(ray_misses (point 1.0 0.0 0.0) (vector 0.0 1.0 0.0))
        Assert.True(ray_misses (point 0.0 0.0 0.0) (vector 0.0 1.0 0.0))
        Assert.True(ray_misses (point 0.0 0.0 -5.0) (vector 1.0 1.0 1.0))

    [<Fact>]
    let ``A ray strikes a cylinder``() =
        let ray_strikes o (v:tuple) t0 t1 =
            let cyl = ShapeCylinder.build_default
            let r = {
                origin = o;
                direction = v.normalize();
            }
            let xs = intersect cyl [] r 
            Assert.Equal(2, List.length xs)
            ((List.item(0) xs).t = t0) && ((List.item(1) xs).t = t1)
        Assert.True(ray_strikes (point 1.0 0.0 -5.0) (vector 0.0 0.0 1.0) 5.0 5.0)
        Assert.True(ray_strikes (point 0.0 0.0 -5.0) (vector 0.0 0.0 1.0) 4.0 6.0)
        Assert.True(ray_strikes (point 0.5 0.0 -5.0) (vector 0.1 1.0 1.0) 6.80798191702732 7.0887234393788612)

    [<Fact>]
    let ``Normal vector on a cylinder``() =
        let cyl = ShapeCylinder.build_default
        Assert.Equal(vector 1.0 0.0 0.0, normal_at None cyl [] (point 1.0 0.0 0.0))
        Assert.Equal(vector 0.0 0.0 -1.0, normal_at None cyl [] (point 0.0 5.0 -1.0))
        Assert.Equal(vector 0.0 0.0 1.0, normal_at None cyl [] (point 0.0 -2.0 1.0))
        Assert.Equal(vector -1.0 0.0 0.0, normal_at None cyl [] (point -1.0 1.0 0.0))

    [<Fact>]
    let ``Intersecting a constrained cylinder``() =
        let f p (d:tuple) =
            let cyl = ShapeCylinder.build 1.0 2.0 false
            let r = {
                origin = p;
                direction = d.normalize();
            }
            let xs = intersect cyl [] r
            Seq.length xs
        Assert.Equal(0, f (point 0.0 1.5 0.0) (vector 0.1 1.0 0.0))
        Assert.Equal(0, f (point 0.0 3.0 -5.0) (vector 0.0 0.0 1.0))
        Assert.Equal(0, f (point 0.0 0.0 -5.0) (vector 0.0 0.0 1.0))
        Assert.Equal(0, f (point 0.0 2.0 -5.0) (vector 0.0 0.0 1.0))
        Assert.Equal(0, f (point 0.0 1.0 -5.0) (vector 0.0 0.0 1.0))
        Assert.Equal(2, f (point 0.0 1.5 -2.0) (vector 0.0 0.0 1.0))

    [<Fact>]
    let ``Intersecting the caps of a closed cylinder``() =
        let f p (d:tuple) =
            let cyl = ShapeCylinder.build 1.0 2.0 true
            let r = {
                origin = p;
                direction = d.normalize();
            }
            let xs = intersect cyl [] r
            Seq.length xs
        Assert.Equal(2, f (point 0.0 3.0 0.0) (vector 0.0 -1.0 0.0))
        Assert.Equal(2, f (point 0.0 3.0 -2.0) (vector 0.0 -1.0 2.0))
        Assert.Equal(2, f (point 0.0 4.0 -2.0) (vector 0.0 -1.0 1.0))
        Assert.Equal(2, f (point 0.0 0.0 -2.0) (vector 0.0 1.0 2.0))
        Assert.Equal(2, f (point 0.0 -1.0 -2.0) (vector 0.0 1.0 1.0))

    [<Fact>]
    let ``The normal vector on a cylinder's end caps``() = 
        let f p =
            let cyl = ShapeCylinder.build 1.0 2.0 true
            normal_at None cyl [] p
        Assert.Equal(vector 0.0 -1.0 0.0, f (point 0.0 1.0 0.0))
        Assert.Equal(vector 0.0 -1.0 0.0, f (point 0.5 1.0 0.0))
        Assert.Equal(vector 0.0 -1.0 0.0, f (point 0.0 1.0 0.5))
        Assert.Equal(vector 0.0 1.0 0.0, f (point 0.0 2.0 0.0))
        Assert.Equal(vector 0.0 1.0 0.0, f (point 0.5 2.0 0.0))
        Assert.Equal(vector 0.0 1.0 0.0, f (point 0.0 2.0 0.5))