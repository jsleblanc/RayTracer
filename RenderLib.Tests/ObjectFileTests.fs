namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Shapes
open RenderLib.Ray
open RenderLib.ObjectFiles

module ObjectFileTests =

    let triangles_obj () =
        "v -1 1 0
        v -1 0 0
        v 1 0 0
        v 1 1 0
        
        g FirstGroup
        f 1 2 3
        g SecondGroup
        f 1 3 4"

    [<Fact>]
    let ``Ignoring unrecognized lines``() =
        let gibberish = 
            "There was a young lady named Bright
            who traveled much faster than light.
            She set out one day
            in a relatrive way,
            and came back the previous night."
        let result = parse_text gibberish
        Assert.Equal(0, List.length result.defaultGroup)

    [<Fact>]
    let ``Vertex records``() =
        let text = 
            "v -1 1 0
            v -1.0000 0.5000 0.0000
            v 1 0 0
            v 1 1 0"
        let result = parse_text text
        Assert.Equal(point -1.0 1.0 0.0, result.vertices.[1])
        Assert.Equal(point -1.0 0.5 0.0, result.vertices.[2])
        Assert.Equal(point 1.0 0.0 0.0, result.vertices.[3])
        Assert.Equal(point 1.0 1.0 0.0, result.vertices.[4])

    [<Fact>]
    let ``Parsing triangle faces``() =
        let text = 
            "v -1 1 0
            v -1 0 0
            v 1 0 0
            v 1 1 0
            f 1 2 3
            f 1 3 4"
        let result = parse_text text
        let children = result.defaultGroup
        let t1 = ShapeTriangle.data children.[0]
        let t2 = ShapeTriangle.data children.[1]
        Assert.Equal(result.vertices.[1], t1.p1)
        Assert.Equal(result.vertices.[2], t1.p2)
        Assert.Equal(result.vertices.[3], t1.p3)
        Assert.Equal(result.vertices.[1], t2.p1)
        Assert.Equal(result.vertices.[3], t2.p2)
        Assert.Equal(result.vertices.[4], t2.p3)

    [<Fact>]
    let ``Triangulating polygons``() =
        let text = 
            "v -1 1 0
            v -1 0 0
            v 1 0 0
            v 1 1 0
            v 0 2 0
            f 1 2 3 4 5"
        let result = parse_text text
        let children = result.defaultGroup
        let t1 = ShapeTriangle.data children.[0]
        let t2 = ShapeTriangle.data children.[1]
        let t3 = ShapeTriangle.data children.[2]
        Assert.Equal(result.vertices.[1], t1.p1)
        Assert.Equal(result.vertices.[2], t1.p2)
        Assert.Equal(result.vertices.[3], t1.p3)
        Assert.Equal(result.vertices.[1], t2.p1)
        Assert.Equal(result.vertices.[3], t2.p2)
        Assert.Equal(result.vertices.[4], t2.p3)
        Assert.Equal(result.vertices.[1], t3.p1)
        Assert.Equal(result.vertices.[4], t3.p2)
        Assert.Equal(result.vertices.[5], t3.p3)

    [<Fact>]
    let ``Triangles in groups``() =            
        let result = parse_text (triangles_obj ())
        let g1_children = result.namedGroups.["FirstGroup"]
        let g2_children = result.namedGroups.["SecondGroup"]       
        let t1 = ShapeTriangle.data g1_children.[0]
        let t2 = ShapeTriangle.data g2_children.[0]
        Assert.Equal(result.vertices.[1], t1.p1)
        Assert.Equal(result.vertices.[2], t1.p2)
        Assert.Equal(result.vertices.[3], t1.p3)
        Assert.Equal(result.vertices.[1], t2.p1)
        Assert.Equal(result.vertices.[3], t2.p2)
        Assert.Equal(result.vertices.[4], t2.p3)

    [<Fact>]
    let ``Converting an OBJ file to a group``() =
        let result = parse_text (triangles_obj ())
        let g = result_to_group result
        let children = ShapeGroup.get_children g
        Assert.True(List.contains (ShapeGroup.build result.namedGroups.["FirstGroup"]) children)
        Assert.True(List.contains (ShapeGroup.build result.namedGroups.["SecondGroup"]) children)

    [<Fact>]
    let ``Vertex normal records``() =
        let text = 
            "vn 0 0 1
            vn 0.707 0 -0.707
            vn 1 2 3"
        let result = parse_text text
        Assert.Equal(vector 0.0 0.0 1.0, result.normals.[1])
        Assert.Equal(vector 0.707 0.0 -0.707, result.normals.[2])
        Assert.Equal(vector 1.0 2.0 3.0, result.normals.[3])

    [<Fact>]
    let ``Faces with normals``() =
        let text = 
            "v 0 1 0
            v -1.0 0 0
            v 1 0 0
            vn -1 0 0
            vn 1 0 0
            vn 0 1 0
            f 1//3 2//3 3//2
            f 1/0/3 2/102/1 3/14/2"
        let result = parse_text text
        let g = result_to_group result
        let s1 = List.item(0) (ShapeGroup.get_children g)
        let s2 = List.item(1) (ShapeGroup.get_children g)
        let t1 = ShapeTriangle.data s1
        let t2 = ShapeTriangle.data s2
        Assert.Equal(result.vertices.[1], t1.p1)
        Assert.Equal(result.vertices.[2], t1.p2)
        Assert.Equal(result.vertices.[3], t1.p3)
        Assert.Equal(result.normals.[3], t1.n1)
        Assert.Equal(result.normals.[1], t1.n2)
        Assert.Equal(result.normals.[2], t1.n3)
        Assert.Equal(t1,t2)
