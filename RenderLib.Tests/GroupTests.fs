namespace RenderLib.Tests

open Xunit
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
