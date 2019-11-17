namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Shapes2
open RenderLib.Ray

module ConeTests =

    [<Fact>]
    let ``Intersecting a cone with a ray``() =
        let ray_strikes o (v:tuple) t0 t1 =
            let c = ShapeCone.build_default
            let r = {
                origin = o;
                direction = v.normalize()
            }
            let xs = c.local_intersect c [] r
            Assert.Equal(2, List.length xs)
            ((List.item(0) xs).t = t0) && ((List.item(1) xs).t = t1)
        Assert.True(ray_strikes (point 0.0 0.0 -5.0) (vector 0.0 0.0 1.0) 5.0 5.0)
        Assert.True(ray_strikes (point 0.0 0.0 -5.0) (vector 1.0 1.0 1.0) 8.6602540378443855 8.6602540378443855)
        Assert.True(ray_strikes (point 1.0 1.0 -5.0) (vector -0.5 -1.0 1.0) 4.5500556793563494 49.449944320643645)

    [<Fact>]
    let ``Intersecting a cone with a ray parallel to one if its halves``() =
        let c = ShapeCone.build_default
        let r = {
            origin = point 0.0 0.0 -1.0;
            direction = (vector 0.0 1.0 1.0).normalize();
        }
        let xs = c.local_intersect c [] r
        Assert.Equal(1, List.length xs)
        Assert.Equal(0.3535533905932738, (List.item(0) xs).t)

    [<Fact>]
    let ``Intersecting a cones end caps``() =
        let ray_strikes o (d:tuple) =
            let c = ShapeCone.build -0.5 0.5 true
            let r = {
                origin = o;
                direction = d.normalize();
            }
            let xs = c.local_intersect c [] r
            List.length xs
        Assert.Equal(0, ray_strikes (point 0.0 0.0 -5.0) (vector 0.0 1.0 0.0))
        Assert.Equal(2, ray_strikes (point 0.0 0.0 -0.25) (vector 0.0 1.0 1.0))
        Assert.Equal(4, ray_strikes (point 0.0 0.0 -0.25) (vector 0.0 1.0 0.0))

    [<Fact>]
    let ``Computing the normal vector on a cone``() =
        let c = ShapeCone.build Double.NegativeInfinity Double.PositiveInfinity true
        Assert.Equal(vector 0.0 0.0 0.0, c.local_normal_at None c (point 0.0 0.0 0.0))
        Assert.Equal(vector 1.0 (-Math.Sqrt(2.0)) 1.0, c.local_normal_at None c (point 1.0 1.0 1.0))
        Assert.Equal(vector -1.0 1.0 0.0, c.local_normal_at None c (point -1.0 -1.0 0.0))       
