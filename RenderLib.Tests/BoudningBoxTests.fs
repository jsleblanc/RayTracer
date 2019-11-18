namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Shapes2
open RenderLib.Translations
open RenderLib.Ray
open RenderLib.Material
open System.Collections.Generic

module BoudningBoxTests =

    [<Fact>]
    let ``Adding a point to a bounding box should re-evaluate minimum and maximum``() =
        let b = 
            BoundingBoxes.build_default
            |> BoundingBoxes.add_point (point -5.0 2.0 0.0)
            |> BoundingBoxes.add_point (point 7.0 0.0 -3.0)
        Assert.Equal(point -5.0 0.0 -3.0, b.minimum)
        Assert.Equal(point 7.0 2.0 0.0, b.maximum)

    [<Fact>]
    let ``An unbounded cylinder has a bouding box`` () =
        let c = ShapeCylinder.build_default
        let b = c.bounds_of c
        Assert.Equal(point -1.0 Double.NegativeInfinity -1.0, b.minimum)
        Assert.Equal(point 1.0 Double.PositiveInfinity 1.0, b.maximum)

    [<Fact>]
    let ``A bounded cylinder has a bounding box``() =
        let c = ShapeCylinder.build -5.0 3.0 false
        let b = c.bounds_of c
        Assert.Equal(point -1.0 -5.0 -1.0, b.minimum)
        Assert.Equal(point 1.0 3.0 1.0, b.maximum)

    [<Fact>]
    let ``An unbounded cone has a bounding box``() =
        let c = ShapeCone.build_default
        let b = c.bounds_of c
        Assert.Equal(point Double.NegativeInfinity Double.NegativeInfinity Double.NegativeInfinity, b.minimum)
        Assert.Equal(point Double.PositiveInfinity Double.PositiveInfinity Double.PositiveInfinity, b.maximum)

    [<Fact>]
    let ``A bounded cone has a bouding box``() =
        let c = ShapeCone.build -5.0 3.0 false
        let b = c.bounds_of c
        Assert.Equal(point -5.0 -5.0 -5.0, b.minimum)
        Assert.Equal(point 5.0 3.0 5.0, b.maximum)

    [<Fact>]
    let ``Adding one bounding box to another``() =
        let box1 = BoundingBoxes.build (point -5.0 -2.0 0.0) (point 7.0 4.0 4.0)
        let box2 = BoundingBoxes.build (point 8.0 -7.0 -2.0) (point 14.0 2.0 8.0)
        let a = BoundingBoxes.add_boxes box2 box1
        Assert.Equal(point -5.0 -7.0 -2.0, a.minimum)
        Assert.Equal(point 14.0 4.0 8.0, a.maximum)

    [<Fact>]
    let ``Checking to see if a box contains a given point``() =
        let box = BoundingBoxes.build (point 5.0 -2.0 0.0) (point 11.0 4.0 7.0)
        Assert.True(BoundingBoxes.contains_point box (point 5.0 -2.0 0.0))
        Assert.True(BoundingBoxes.contains_point box (point 11.0 4.0 7.0))
        Assert.True(BoundingBoxes.contains_point box (point 8.0 1.0 3.0))
        Assert.False(BoundingBoxes.contains_point box (point 3.0 0.0 3.0))
        Assert.False(BoundingBoxes.contains_point box (point 8.0 -4.0 3.0))
        Assert.False(BoundingBoxes.contains_point box (point 8.0 1.0 -1.0))
        Assert.False(BoundingBoxes.contains_point box (point 13.0 1.0 3.0))
        Assert.False(BoundingBoxes.contains_point box (point 8.0 5.0 3.0))
        Assert.False(BoundingBoxes.contains_point box (point 8.0 1.0 8.0))

    [<Fact>]
    let ``Checking to see if a box contains a given box``() =
        let box = BoundingBoxes.build (point 5.0 -2.0 0.0) (point 11.0 4.0 7.0)
        Assert.True(BoundingBoxes.contains_box box { minimum = point 5.0 -2.0 0.0; maximum = point 11.0 4.0 7.0; })
        Assert.True(BoundingBoxes.contains_box box { minimum = point 6.0 -1.0 1.0; maximum = point 10.0 3.0 6.0; })
        Assert.False(BoundingBoxes.contains_box box { minimum = point 4.0 -3.0 -1.0; maximum = point 10.0 3.0 6.0; })
        Assert.False(BoundingBoxes.contains_box box { minimum = point 6.0 -1.0 1.0; maximum = point 12.0 5.0 8.0; })

    [<Fact>]
    let ``Transforming a bounding box``() =
        let box = 
            BoundingBoxes.build (point -1.0 -1.0 -1.0) (point 1.0 1.0 1.0)
            |> BoundingBoxes.transform (rotation_x (Math.PI/4.0) * rotation_y (Math.PI/4.0))
        Assert.Equal(point -1.414213562 -1.707106781 -1.707106781, box.minimum)
        Assert.Equal(point 1.414213562 1.707106781 1.707106781, box.maximum)
        (*
    [<Fact>]
    let ``Querying a shape's bounding box in its parents's space``() =
        let s = ShapeSphere.build |> Shapes2.transform ((translation 1.0 -3.0 5.0) * (scaling 0.5 2.0 4.0))
        let box = parent_space_bounds_of s
        Assert.Equal(point 0.5 -5.0 1.0, box.minimum)
        Assert.Equal(point 1.5 -1.0 9.0, box.maximum)
        *)
    [<Fact>]
    let ``A group has a bounding box that contains its children``() =
        let s = ShapeSphere.build |> Shapes2.transform ((translation 2.0 5.0 -3.0) * (scaling 2.0 2.0 2.0))
        let c = ShapeCylinder.build -2.0 2.0 false |> Shapes2.transform ((translation -4.0 -1.0 4.0) * (scaling 0.5 1.0 0.5))
        let g = ShapeGroup.build [s;c;]
        let box = g.bounds_of g
        Assert.Equal(point -4.5 -3.0 -5.0, box.minimum)
        Assert.Equal(point 4.0 7.0 4.5, box.maximum)

    [<Fact>]
    let ``Intersecting a ray with a bounding box at the origin``() =
        let func o (d:tuple) =
            let box = BoundingBoxes.build (point -1.0 -1.0 -1.0) (point 1.0 1.0 1.0)
            let n = d.normalize()
            let r = {
                origin = o;
                direction = n;
            }
            BoundingBoxes.intersects box r
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
            let box = BoundingBoxes.build (point 5.0 -2.0 0.0) (point 11.0 4.0 7.0)
            let n = d.normalize()
            let r = {
                origin = o;
                direction = n;
            }
            BoundingBoxes.intersects box r
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

