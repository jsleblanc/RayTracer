namespace RenderLib.Tests

open Xunit
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Ray
open RenderLib.Shapes2
open RenderLib.Translations

module CubeTests = 

    [<Fact>]        
    let ``A ray intersects a cube``() =
        let ray_intersects_cube o d t1 t2 =
            let ray = {
                origin = o;
                direction = d;
            }
            let c = RenderLib.ShapeCube.build
            let xs = intersect c [] ray
            let a = (Seq.length xs) = 2
            let b = (Seq.item(0) xs).t = t1
            let c = (Seq.item(1) xs).t = t2
            a && b && c
        Assert.True(ray_intersects_cube (point 5.0 0.5 0.0) (vector -1.0 0.0 0.0) 4.0 6.0, "+x")
        Assert.True(ray_intersects_cube (point -5.0 0.5 0.0) (vector 1.0 0.0 0.0) 4.0 6.0, "-x")
        Assert.True(ray_intersects_cube (point 0.5 5.0 0.0) (vector 0.0 -1.0 0.0) 4.0 6.0, "+y")
        Assert.True(ray_intersects_cube (point 0.5 -5.0 0.0) (vector 0.0 1.0 0.0) 4.0 6.0, "-y")
        Assert.True(ray_intersects_cube (point 0.5 0.0 5.0) (vector 0.0 0.0 -1.0) 4.0 6.0, "+z")
        Assert.True(ray_intersects_cube (point 0.5 0.0 -5.0) (vector 0.0 0.0 1.0) 4.0 6.0, "-z")
        Assert.True(ray_intersects_cube (point 0.0 0.5 0.0) (vector 0.0 0.0 1.0) -1.0 1.0, "inside")

    [<Fact>]
    let ``A ray misses a cube``() =
        let ray_misses_cube o d =
            let c = RenderLib.ShapeCube.build
            let r = {
                origin = o;
                direction = d;
            }
            let xs = intersect c [] r
            (Seq.length xs) = 0
        Assert.True(ray_misses_cube (point -2.0 0.0 0.0) (vector 0.2673 0.5345 0.8018), "1")
        Assert.True(ray_misses_cube (point 0.0 -2.0 0.0) (vector 0.8018 0.2673 0.5345), "2")
        Assert.True(ray_misses_cube (point 0.0 0.0 -2.0) (vector 0.5345 0.8018 0.2673), "3")
        Assert.True(ray_misses_cube (point 2.0 0.0 2.0) (vector 0.0 0.0 -1.0), "4")
        Assert.True(ray_misses_cube (point 0.0 2.0 2.0) (vector 0.0 -1.0 0.0), "5")
        Assert.True(ray_misses_cube (point 2.0 2.0 0.0) (vector -1.0 0.0 0.0), "6")

    [<Fact>]
    let ``The normal on the surface of a cube``() =
        let c = RenderLib.ShapeCube.build
        Assert.Equal(vector 1.0 0.0 0.0, normal_at None c [] (point 1.0 0.5 -0.8))
        Assert.Equal(vector -1.0 0.0 0.0, normal_at None c [] (point -1.0 -0.2 0.9))
        Assert.Equal(vector 0.0 1.0 0.0, normal_at None c [] (point -0.4 1.0 -0.1))
        Assert.Equal(vector 0.0 -1.0 0.0, normal_at None c [] (point 0.3 -1.0 -0.7))
        Assert.Equal(vector 0.0 0.0 1.0, normal_at None c [] (point -0.6 0.3 1.0))
        Assert.Equal(vector 0.0 0.0 -1.0, normal_at None c [] (point 0.4 0.4 -1.0))
        Assert.Equal(vector 1.0 0.0 0.0, normal_at None c [] (point 1.0 1.0 1.0))
        Assert.Equal(vector -1.0 0.0 0.0, normal_at None c [] (point -1.0 -1.0 -1.0))
