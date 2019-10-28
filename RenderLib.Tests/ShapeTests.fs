namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Ray

module ShapeTests = 
    
    [<Fact>]
    let ``A sphere's default transformation``() =
        let s = Sphere(shapeProperties.Default)
        let st = shapeTransformation s
        Assert.Equal(identity_matrix(), st)

    [<Fact>]
    let ``Changing a sphere's transformation``() =
        let t = translation 2.0 3.0 4.0
        let s = Sphere({ shapeProperties.Default with default_transformation = t; })
        let st = shapeTransformation s
        Assert.Equal(t, st)

    [<Fact>]
    let ``Intersecting a scaled sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let x = Sphere { shapeProperties.Default with default_transformation = scaling 2.0 2.0 2.0; }
        let xs = intersect x r
        Assert.Equal(2, Seq.length xs)
        Assert.Equal(3.0, (Seq.item 0 xs).t)
        Assert.Equal(7.0, (Seq.item 1 xs).t)

    [<Fact>]
    let ``Intersecting a translated sphere with a ray``() =
        let r = { origin = point 0.0 0.0 -5.0; direction = vector 0.0 0.0 1.0; }
        let x = Sphere { shapeProperties.Default with default_transformation = translation 5.0 0.0 0.0; }
        let xs = intersect x r
        Assert.True(Seq.isEmpty xs)           

    module SphereTests = 
        
        [<Fact>]
        let ``The normal on a sphere at a point on the x axis``() =
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point 1.0 0.0 0.0)
            Assert.Equal(vector 1.0 0.0 0.0, n)

        [<Fact>]
        let ``The normal on a sphere at a point on the y axis``() =
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point 0.0 1.0 0.0)
            Assert.Equal(vector 0.0 1.0 0.0, n)

        [<Fact>]
        let ``The normal on a sphere at a point on the z axis``() =
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point 0.0 0.0 1.0)
            Assert.Equal(vector 0.0 0.0 1.0, n)

        [<Fact>]
        let ``The normal on a sphere at a nonaxial point``() =
            let v = Math.Sqrt(3.0) / 3.0
            let s = Sphere(shapeProperties.Default)
            let n = normal_at s (point v v v)
            Assert.Equal(vector v v v, n)

        [<Fact>]
        let ``The normal is a normalized vector``() =
            let s = Sphere(shapeProperties.Default)
            let v = Math.Sqrt(3.0) / 3.0
            let n = normal_at s (point v v v)
            Assert.Equal(n.normalize(), n)

        [<Fact>]
        let ``Computing the normal on a translated sphere``() =
            let x = Sphere { shapeProperties.Default with default_transformation = translation 0.0 1.0 0.0; }                
            let n = normal_at x (point 0.0 1.70711 -0.70711)
            Assert.Equal(vector 0.0 0.7071067812 -0.7071067812, roundtuple n)

        [<Fact>]
        let ``Computing the normal on a transformed sphere``() =
            let x = Sphere { shapeProperties.Default with default_transformation = (scaling 1.0 0.5 1.0) * rotation_z (Math.PI / 5.0); }
            let n = normal_at x (point 0.0 (Math.Sqrt(2.0)/2.0) (-Math.Sqrt(2.0)/2.0))
            Assert.Equal(vector 0.0 0.97014250014533188 -0.24253562503633294, n)
    
    module PlanesTests =

        [<Fact>]
        let ``The normal of a plane is a constant everywhere``() =
            let p = Plane(shapeProperties.Default)
            let n1 = normal_at p (point 0.0 0.0 0.0)
            let n2 = normal_at p (point 10.0 0.0 -10.0)
            let n3 = normal_at p (point -5.0 0.0 150.0)
            let v = vector 0.0 1.0 0.0
            Assert.Equal(v, n1)
            Assert.Equal(v, n2)
            Assert.Equal(v, n3)
    
        [<Fact>]
        let ``Intersect with a ray parallel to the plane``() =
            let p = Plane(shapeProperties.Default)
            let r = {
                origin = point 0.0 10.0 0.0;
                direction = vector 0.0 0.0 1.0;
            }
            let xs = intersect p r
            Assert.True(Seq.isEmpty xs)

        [<Fact>]
        let ``Intersect with a coplanar ray``() =
            let p = Plane(shapeProperties.Default)
            let r = {
                origin = point 0.0 0.0 0.0;
                direction = vector 0.0 0.0 1.0;
            }
            let xs = intersect p r
            Assert.True(Seq.isEmpty xs)

        [<Fact>]
        let ``A ray intersecting a plane from above``() =
            let p = Plane(shapeProperties.Default)
            let r = {
                origin = point 0.0 1.0 0.0;
                direction = vector 0.0 -1.0 0.0;
            }
            let xs = intersect p r
            Assert.Equal(1, Seq.length xs)
            Assert.Equal(1.0, (Seq.item 0 xs).t)
            Assert.Equal(p, (Seq.item 0 xs).obj)

        [<Fact>]
        let ``A ray intersecting a plane from below``() =
            let p = Plane(shapeProperties.Default)
            let r = {
                origin = point 0.0 -1.0 0.0;
                direction = vector 0.0 1.0 0.0;
            }
            let xs = intersect p r
            Assert.Equal(1, Seq.length xs)
            Assert.Equal(1.0, (Seq.item 0 xs).t)
            Assert.Equal(p, (Seq.item 0 xs).obj)

    module CubeTests = 

        [<Fact>]        
        let ``A ray intersects a cube``() =
            let ray_intersects_cube o d t1 t2 =
                let ray = {
                    origin = o;
                    direction = d;
                }
                let c = Cube(shapeProperties.Default)
                let xs = intersect c ray
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
                let c = Cube(shapeProperties.Default)
                let r = {
                    origin = o;
                    direction = d;
                }
                let xs = intersect c r
                (Seq.length xs) = 0
            Assert.True(ray_misses_cube (point -2.0 0.0 0.0) (vector 0.2673 0.5345 0.8018), "1")
            Assert.True(ray_misses_cube (point 0.0 -2.0 0.0) (vector 0.8018 0.2673 0.5345), "2")
            Assert.True(ray_misses_cube (point 0.0 0.0 -2.0) (vector 0.5345 0.8018 0.2673), "3")
            Assert.True(ray_misses_cube (point 2.0 0.0 2.0) (vector 0.0 0.0 -1.0), "4")
            Assert.True(ray_misses_cube (point 0.0 2.0 2.0) (vector 0.0 -1.0 0.0), "5")
            Assert.True(ray_misses_cube (point 2.0 2.0 0.0) (vector -1.0 0.0 0.0), "6")

        [<Fact>]
        let ``The normal on the surface of a cube``() =
            let c = Cube(shapeProperties.Default)
            Assert.Equal(vector 1.0 0.0 0.0, normal_at c (point 1.0 0.5 -0.8))
            Assert.Equal(vector -1.0 0.0 0.0, normal_at c (point -1.0 -0.2 0.9))
            Assert.Equal(vector 0.0 1.0 0.0, normal_at c (point -0.4 1.0 -0.1))
            Assert.Equal(vector 0.0 -1.0 0.0, normal_at c (point 0.3 -1.0 -0.7))
            Assert.Equal(vector 0.0 0.0 1.0, normal_at c (point -0.6 0.3 1.0))
            Assert.Equal(vector 0.0 0.0 -1.0, normal_at c (point 0.4 0.4 -1.0))
            Assert.Equal(vector 1.0 0.0 0.0, normal_at c (point 1.0 1.0 1.0))
            Assert.Equal(vector -1.0 0.0 0.0, normal_at c (point -1.0 -1.0 -1.0))

    module CylinderTests = 

        [<Fact>]
        let ``A ray misses a cylinder``() =
            let ray_misses o (v:tuple) =
                let cyl = Cylinder(shapeProperties.Default, Double.NegativeInfinity, Double.PositiveInfinity, false)
                let direction = v.normalize()
                let r = {
                    origin = o;
                    direction = direction;
                }
                let xs = intersect cyl r 
                Seq.length xs = 0
            Assert.True(ray_misses (point 1.0 0.0 0.0) (vector 0.0 1.0 0.0))
            Assert.True(ray_misses (point 0.0 0.0 0.0) (vector 0.0 1.0 0.0))
            Assert.True(ray_misses (point 0.0 0.0 -5.0) (vector 1.0 1.0 1.0))

        [<Fact>]
        let ``A ray strikes a cylinder``() =
            let ray_strikes o (v:tuple) t0 t1 =
                let cyl = Cylinder(shapeProperties.Default, Double.NegativeInfinity, Double.PositiveInfinity, false)
                let r = {
                    origin = o;
                    direction = v.normalize();
                }
                let xs = intersect cyl r 
                Assert.Equal(2, Seq.length xs)
                ((Seq.item(0) xs).t = t0) && ((Seq.item(1) xs).t = t1)
            Assert.True(ray_strikes (point 1.0 0.0 -5.0) (vector 0.0 0.0 1.0) 5.0 5.0)
            Assert.True(ray_strikes (point 0.0 0.0 -5.0) (vector 0.0 0.0 1.0) 4.0 6.0)
            Assert.True(ray_strikes (point 0.5 0.0 -5.0) (vector 0.1 1.0 1.0) 6.80798191702732 7.0887234393788612)

        [<Fact>]
        let ``Normal vector on a cylinder``() =
            let cyl = Cylinder(shapeProperties.Default, Double.NegativeInfinity, Double.PositiveInfinity, false)
            Assert.Equal(vector 1.0 0.0 0.0, normal_at cyl (point 1.0 0.0 0.0))
            Assert.Equal(vector 0.0 0.0 -1.0, normal_at cyl (point 0.0 5.0 -1.0))
            Assert.Equal(vector 0.0 0.0 1.0, normal_at cyl (point 0.0 -2.0 1.0))
            Assert.Equal(vector -1.0 0.0 0.0, normal_at cyl (point -1.0 1.0 0.0))

        [<Fact>]
        let ``Intersecting a constrained cylinder``() =
            let f p (d:tuple) =
                let cyl = Cylinder(shapeProperties.Default, 1.0, 2.0, false)
                let r = {
                    origin = p;
                    direction = d.normalize();
                }
                let xs = intersect cyl r
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
                let cyl = Cylinder(shapeProperties.Default,1.0,2.0,true)
                let r = {
                    origin = p;
                    direction = d.normalize();
                }
                let xs = intersect cyl r
                Seq.length xs
            Assert.Equal(2, f (point 0.0 3.0 0.0) (vector 0.0 -1.0 0.0))
            Assert.Equal(2, f (point 0.0 3.0 -2.0) (vector 0.0 -1.0 2.0))
            Assert.Equal(2, f (point 0.0 4.0 -2.0) (vector 0.0 -1.0 1.0))
            Assert.Equal(2, f (point 0.0 0.0 -2.0) (vector 0.0 1.0 2.0))
            Assert.Equal(2, f (point 0.0 -1.0 -2.0) (vector 0.0 1.0 1.0))

        [<Fact>]
        let ``The normal vector on a cylinder's end caps``() = 
            let f p =
                let cyl = Cylinder(shapeProperties.Default,1.0,2.0,true)
                normal_at cyl p
            Assert.Equal(vector 0.0 -1.0 0.0, f (point 0.0 1.0 0.0))
            Assert.Equal(vector 0.0 -1.0 0.0, f (point 0.5 1.0 0.0))
            Assert.Equal(vector 0.0 -1.0 0.0, f (point 0.0 1.0 0.5))
            Assert.Equal(vector 0.0 1.0 0.0, f (point 0.0 2.0 0.0))
            Assert.Equal(vector 0.0 1.0 0.0, f (point 0.5 2.0 0.0))
            Assert.Equal(vector 0.0 1.0 0.0, f (point 0.0 2.0 0.5))

    module ConeTests = 

        [<Fact>]
        let ``Intersecting a cone with a ray``() =
            let ray_strikes o (v:tuple) t0 t1 =
                let c = Cone(shapeProperties.Default,Double.NegativeInfinity,Double.PositiveInfinity,false)
                let r = {
                    origin = o;
                    direction = v.normalize()
                }
                let xs = local_intersect c r
                Assert.Equal(2, Seq.length xs)
                ((Seq.item(0) xs).t = t0) && ((Seq.item(1) xs).t = t1)
            Assert.True(ray_strikes (point 0.0 0.0 -5.0) (vector 0.0 0.0 1.0) 5.0 5.0)
            Assert.True(ray_strikes (point 0.0 0.0 -5.0) (vector 1.0 1.0 1.0) 8.6602540378443855 8.6602540378443855)
            Assert.True(ray_strikes (point 1.0 1.0 -5.0) (vector -0.5 -1.0 1.0) 4.5500556793563494 49.449944320643645)

        [<Fact>]
        let ``Intersecting a cone with a ray parallel to one if its halves``() =
            let c = Cone(shapeProperties.Default,Double.NegativeInfinity,Double.PositiveInfinity,false)
            let r = {
                origin = point 0.0 0.0 -1.0;
                direction = (vector 0.0 1.0 1.0).normalize();
            }
            let xs = local_intersect c r
            Assert.Equal(1, Seq.length xs)
            Assert.Equal(0.3535533905932738, (Seq.item(0) xs).t)

        [<Fact>]
        let ``Intersecting a cones end caps``() =
            let ray_strikes o (d:tuple) =
                let c = Cone(shapeProperties.Default,-0.5,0.5,true)
                let r = {
                    origin = o;
                    direction = d.normalize();
                }
                let xs = local_intersect c r
                Seq.length xs
            Assert.Equal(0, ray_strikes (point 0.0 0.0 -5.0) (vector 0.0 1.0 0.0))
            Assert.Equal(2, ray_strikes (point 0.0 0.0 -0.25) (vector 0.0 1.0 1.0))
            Assert.Equal(4, ray_strikes (point 0.0 0.0 -0.25) (vector 0.0 1.0 0.0))

        [<Fact>]
        let ``Computing the normal vector on a cone``() =
            let c = Cone(shapeProperties.Default,Double.NegativeInfinity,Double.PositiveInfinity,true)
            Assert.Equal(vector 0.0 0.0 0.0, local_normal_at c (point 0.0 0.0 0.0))
            Assert.Equal(vector 1.0 (-Math.Sqrt(2.0)) 1.0, local_normal_at c (point 1.0 1.0 1.0))
            Assert.Equal(vector -1.0 1.0 0.0, local_normal_at c (point -1.0 -1.0 0.0))