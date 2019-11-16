﻿namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Ray
open RenderLib.Material
open System.Collections.Generic

module ShapeTests = 
      
    module CylinderTests = 

        [<Fact>]
        let ``A ray misses a cylinder``() =
            let ray_misses o (v:tuple) =
                let cyl = Cylinder(material.Default,identity_matrix(),None, Double.NegativeInfinity, Double.PositiveInfinity, false)
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
                let cyl = Cylinder(material.Default,identity_matrix(), None,Double.NegativeInfinity, Double.PositiveInfinity, false)
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
            let cyl = Cylinder(material.Default,identity_matrix(), None,Double.NegativeInfinity, Double.PositiveInfinity, false)
            Assert.Equal(vector 1.0 0.0 0.0, normal_at cyl (point 1.0 0.0 0.0))
            Assert.Equal(vector 0.0 0.0 -1.0, normal_at cyl (point 0.0 5.0 -1.0))
            Assert.Equal(vector 0.0 0.0 1.0, normal_at cyl (point 0.0 -2.0 1.0))
            Assert.Equal(vector -1.0 0.0 0.0, normal_at cyl (point -1.0 1.0 0.0))

        [<Fact>]
        let ``Intersecting a constrained cylinder``() =
            let f p (d:tuple) =
                let cyl = Cylinder(material.Default,identity_matrix(), None,1.0, 2.0, false)
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
                let cyl = Cylinder(material.Default,identity_matrix(),None,1.0,2.0,true)
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
                let cyl = Cylinder(material.Default,identity_matrix(),None,1.0,2.0,true)
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
                let c = Cone(material.Default,identity_matrix(),None,Double.NegativeInfinity,Double.PositiveInfinity,false)
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
            let c = Cone(material.Default,identity_matrix(),None,Double.NegativeInfinity,Double.PositiveInfinity,false)
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
                let c = Cone(material.Default,identity_matrix(),None,-0.5,0.5,true)
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
            let c = Cone(material.Default,identity_matrix(),None,Double.NegativeInfinity,Double.PositiveInfinity,true)
            Assert.Equal(vector 0.0 0.0 0.0, local_normal_at c (point 0.0 0.0 0.0))
            Assert.Equal(vector 1.0 (-Math.Sqrt(2.0)) 1.0, local_normal_at c (point 1.0 1.0 1.0))
            Assert.Equal(vector -1.0 1.0 0.0, local_normal_at c (point -1.0 -1.0 0.0))

    module GroupTests = 

        [<Fact>]
        let ``Adding a child to a group``() =
            let g = Group(Some material.Default,identity_matrix(),None,new HashSet<shape>())
            let s = Sphere(material.Default,identity_matrix(),Some g)
            add_child g s
            Assert.True(has_child g s)
            match s with
            | Sphere (_,_,Some parent) -> Assert.Equal(parent,g)
            | _ -> Assert.False(true, "Shape has no parent or parent isn't group")

        [<Fact>]
        let ``Intersecting a ray with an empty group``() =
            let g = Group(Some material.Default,identity_matrix(),None,new HashSet<shape>())
            let r = {
                origin = point 0.0 0.0 0.0;
                direction = vector 0.0 0.0 1.0;
            }
            let xs = local_intersect g r
            Assert.True(Seq.isEmpty xs)

        [<Fact>]
        let ``Intersecting a ray with a nonempty group``() =
            let g = Group(Some material.Default,identity_matrix(),None,new HashSet<shape>())
            let s1 = Sphere(material.Default,identity_matrix(),Some g)
            let s2 = Sphere(material.Default,translation 0.0 0.0 -3.0,Some g)
            let s3 = Sphere(material.Default,translation 5.0 0.0 0.0,Some g)
            add_child g s1
            add_child g s2
            add_child g s3
            let r = {
                origin = point 0.0 0.0 -5.0;
                direction = vector 0.0 0.0 1.0;
            }
            let xs = local_intersect g r
            Assert.Equal(4, Seq.length xs)
            Assert.Equal((Seq.item(0) xs).obj, s2)
            Assert.Equal((Seq.item(1) xs).obj, s2)
            Assert.Equal((Seq.item(2) xs).obj, s1)
            Assert.Equal((Seq.item(3) xs).obj, s1)

        [<Fact>]
        let ``Intersecting a transformed group``() =
            let g = Group(Some material.Default,scaling 2.0 2.0 2.0,None,new HashSet<shape>())
            let s = Sphere(material.Default,translation 5.0 0.0 0.0,Some g)
            add_child g s
            let r = {
                origin = point 10.0 0.0 -10.0;
                direction = vector 0.0 0.0 1.0;
            }
            let xs = intersect g r
            Assert.Equal(2, Seq.length xs)

        [<Fact>]
        let ``Converting a point from world to object space``() =
            let g1 = Group(Some material.Default,rotation_y (Math.PI/2.0),None,new HashSet<shape>())
            let g2 = Group(Some material.Default,scaling 2.0 2.0 2.0,Some g1,new HashSet<shape>())
            add_child g1 g2
            let s = Sphere(material.Default,translation 5.0 0.0 0.0,Some g2)
            add_child g2 s
            let p = world_to_object s (point -2.0 0.0 -10.0)
            Assert.Equal(point 0.0 0.0 -1.0, p)

        [<Fact>]
        let ``Converting a normal from object to world space``() =
            let g1 = Group(Some material.Default,rotation_y (Math.PI/2.0),None,new HashSet<shape>())
            let g2 = Group(Some material.Default,scaling 1.0 2.0 3.0,Some g1,new HashSet<shape>())
            add_child g1 g2
            let s = Sphere(material.Default,translation 5.0 0.0 0.0,Some g2)
            add_child g2 s
            let q = (Math.Sqrt(3.0)/3.0) 
            let n = normal_to_world s (vector q q q)
            Assert.Equal(vector 0.2857142857 0.4285714286 -0.8571428571, n)

        [<Fact>]
        let ``Finding the nomral on a child object``() =
            let g1 = Group(Some material.Default,rotation_y (Math.PI/2.0),None,new HashSet<shape>())
            let g2 = Group(Some material.Default,scaling 1.0 2.0 3.0,Some g1,new HashSet<shape>())
            add_child g1 g2
            let s = Sphere(material.Default,translation 5.0 0.0 0.0,Some g2)
            add_child g2 s
            let n = normal_at s (point 1.7321 1.1547 -5.5774)
            Assert.Equal(vector 0.2857036818 0.4285431518 -0.8571605294, n)

    module BoudningBoxTests = 

        [<Fact>]
        let ``Adding a point to a bounding box should re-evaluate minimum and maximum``() =
            let b = 
                boundingBox.Default
                |> boundingBoxAddPoint (point -5.0 2.0 0.0)
                |> boundingBoxAddPoint (point 7.0 0.0 -3.0)
            Assert.Equal(point -5.0 0.0 -3.0, b.minimum)
            Assert.Equal(point 7.0 2.0 0.0, b.maximum)

        [<Fact>]
        let ``An unbounded cylinder has a bouding box`` () =
            let c = Cylinder(material.Default,identity_matrix(),None,Double.NegativeInfinity,Double.PositiveInfinity,false)
            let b = bounds_of c
            Assert.Equal(point -1.0 Double.NegativeInfinity -1.0, b.minimum)
            Assert.Equal(point 1.0 Double.PositiveInfinity 1.0, b.maximum)

        [<Fact>]
        let ``A bounded cylinder has a bounding box``() =
            let c = Cylinder(material.Default,identity_matrix(),None,-5.0,3.0,false)
            let b = bounds_of c
            Assert.Equal(point -1.0 -5.0 -1.0, b.minimum)
            Assert.Equal(point 1.0 3.0 1.0, b.maximum)

        [<Fact>]
        let ``An unbounded cone has a bounding box``() =
            let c = Cone(material.Default,identity_matrix(),None,Double.NegativeInfinity,Double.PositiveInfinity,false)
            let b = bounds_of c
            Assert.Equal(point Double.NegativeInfinity Double.NegativeInfinity Double.NegativeInfinity, b.minimum)
            Assert.Equal(point Double.PositiveInfinity Double.PositiveInfinity Double.PositiveInfinity, b.maximum)

        [<Fact>]
        let ``A bounded cone has a bouding box``() =
            let c = Cone(material.Default,identity_matrix(),None,-5.0,3.0,false)
            let b = bounds_of c
            Assert.Equal(point -5.0 -5.0 -5.0, b.minimum)
            Assert.Equal(point 5.0 3.0 5.0, b.maximum)

        [<Fact>]
        let ``Adding one bounding box to another``() =
            let box1 = { minimum = point -5.0 -2.0 0.0; maximum = point 7.0 4.0 4.0; }
            let box2 = { minimum = point 8.0 -7.0 -2.0; maximum = point 14.0 2.0 8.0; }
            let a = add_bounding_boxes box2 box1
            Assert.Equal(point -5.0 -7.0 -2.0, a.minimum)
            Assert.Equal(point 14.0 4.0 8.0, a.maximum)

        [<Fact>]
        let ``Checking to see if a box contains a given point``() =
            let box = { minimum = point 5.0 -2.0 0.0; maximum = point 11.0 4.0 7.0; }
            Assert.True(box_contains_point box (point 5.0 -2.0 0.0))
            Assert.True(box_contains_point box (point 11.0 4.0 7.0))
            Assert.True(box_contains_point box (point 8.0 1.0 3.0))
            Assert.False(box_contains_point box (point 3.0 0.0 3.0))
            Assert.False(box_contains_point box (point 8.0 -4.0 3.0))
            Assert.False(box_contains_point box (point 8.0 1.0 -1.0))
            Assert.False(box_contains_point box (point 13.0 1.0 3.0))
            Assert.False(box_contains_point box (point 8.0 5.0 3.0))
            Assert.False(box_contains_point box (point 8.0 1.0 8.0))

        [<Fact>]
        let ``Checking to see if a box contains a given box``() =
            let box = { minimum = point 5.0 -2.0 0.0; maximum = point 11.0 4.0 7.0; }
            Assert.True(box_contains_box box { minimum = point 5.0 -2.0 0.0; maximum = point 11.0 4.0 7.0; })
            Assert.True(box_contains_box box { minimum = point 6.0 -1.0 1.0; maximum = point 10.0 3.0 6.0; })
            Assert.False(box_contains_box box { minimum = point 4.0 -3.0 -1.0; maximum = point 10.0 3.0 6.0; })
            Assert.False(box_contains_box box { minimum = point 6.0 -1.0 1.0; maximum = point 12.0 5.0 8.0; })

        [<Fact>]
        let ``Transforming a bounding box``() =
            let box = { minimum = point -1.0 -1.0 -1.0; maximum = point 1.0 1.0 1.0; }
            let m = rotation_x (Math.PI/4.0) * rotation_y (Math.PI/4.0)
            let box2 = transform_box box m
            Assert.Equal(point -1.414213562 -1.707106781 -1.707106781, box2.minimum)
            Assert.Equal(point 1.414213562 1.707106781 1.707106781, box2.maximum)

        [<Fact>]
        let ``Querying a shape's bounding box in its parents's space``() =
            let s = Sphere(material.Default,(translation 1.0 -3.0 5.0) * (scaling 0.5 2.0 4.0),None)
            let box = parent_space_bounds_of s
            Assert.Equal(point 0.5 -5.0 1.0, box.minimum)
            Assert.Equal(point 1.5 -1.0 9.0, box.maximum)

        [<Fact>]
        let ``A group has a bounding box that contains its children``() =
            let g = Group(Some material.Default,identity_matrix(),None,new HashSet<shape>())
            let s = Sphere(material.Default,(translation 2.0 5.0 -3.0) * (scaling 2.0 2.0 2.0),Some g)
            let c = Cylinder(material.Default,(translation -4.0 -1.0 4.0) * (scaling 0.5 1.0 0.5),Some g,-2.0,2.0,false)
            add_child g s
            add_child g c
            let box = bounds_of g
            Assert.Equal(point -4.5 -3.0 -5.0, box.minimum)
            Assert.Equal(point 4.0 7.0 4.5, box.maximum)

        [<Fact>]
        let ``Intersecting a ray with a bounding box at the origin``() =
            let func o (d:tuple) =
                let box = {
                    minimum = point -1.0 -1.0 -1.0;
                    maximum = point 1.0 1.0 1.0;
                }
                let n = d.normalize()
                let r = {
                    origin = o;
                    direction = n;
                }
                intersects box r
            Assert.True(func (point 5.0 0.5 0.0) (vector -1.0 0.0 0.0))
            Assert.True(func (point -5.0 0.5 0.0) (vector 1.0 0.0 0.0))
            Assert.True(func (point 0.5 5.0 0.0) (vector 0.0 -1.0 0.0))
            Assert.True(func (point 0.5 -5.0 0.0) (vector 0.0 1.0 0.0))
            Assert.True(func (point 0.5 0.0 5.0) (vector 0.0 0.0 -1.0))
            Assert.True(func (point 0.5 0.0 -5.0) (vector 0.0 0.0 1.0))
            Assert.True(func (point 0.0 0.5 0.0) (vector 0.0 0.0 1.0))
            Assert.False(func (point -2.0 0.0 0.0) (vector 2.0 4.0 6.0))
            Assert.False(func (point 0.0 -2.0 0.0) (vector 6.0 2.0 4.0)) 
            Assert.False(func (point 0.0 0.0 -2.0) (vector 4.0 6.0 2.0)) 
            Assert.False(func (point 2.0 0.0 2.0) (vector 0.0 0.0 -1.0))
            Assert.False(func (point 0.0 2.0 2.0) (vector 0.0 -1.0 0.0))
            Assert.False(func (point 2.0 2.0 0.0) (vector -1.0 0.0 0.0))

        [<Fact>]
        let ``Intersecting a ray with a non-cubic bounding box``() =
            let func o (d:tuple) =
                let box = {
                    minimum = point 5.0 -2.0 0.0;
                    maximum = point 11.0 4.0 7.0;
                }
                let n = d.normalize()
                let r = {
                    origin = o;
                    direction = n;
                }
                intersects box r
            Assert.True(func (point 15.0 1.0 2.0) (vector -1.0 0.0 0.0))
            Assert.True(func (point -5.0 -1.0 4.0) (vector 1.0 0.0 0.0))
            Assert.True(func (point 7.0 6.0 5.0) (vector 0.0 -1.0 0.0))
            Assert.True(func (point 9.0 -5.0 6.0) (vector 0.0 1.0 0.0))
            Assert.True(func (point 8.0 2.0 12.0) (vector 0.0 0.0 -1.0))
            Assert.True(func (point 6.0 0.0 -5.0) (vector 0.0 0.0 1.0))
            Assert.True(func (point 8.0 1.0 3.5) (vector 0.0 0.0 1.0))
            Assert.False(func (point 9.0 -1.0 -8.0) (vector 2.0 4.0 6.0))
            Assert.False(func (point 8.0 3.0 -4.0) (vector 6.0 2.0 4.0))
            Assert.False(func (point 9.0 -1.0 -2.0) (vector 4.0 6.0 2.0))
            Assert.False(func (point 4.0 0.0 9.0) (vector 0.0 0.0 -1.0))
            Assert.False(func (point 8.0 6.0 -1.0) (vector 0.0 -1.0 0.0))
            Assert.False(func (point 12.0 5.0 4.0) (vector -1.0 0.0 0.0))