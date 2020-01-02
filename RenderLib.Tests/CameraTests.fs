namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Matrix
open RenderLib.Color
open RenderLib.Translations
open RenderLib.Material
open RenderLib.Camera

module CameraTests = 

    let default_world = 
        let s1 = 
            ShapeSphere.build
            |> Shapes.texture { Material.material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }
        let s2 = 
            ShapeSphere.build
            |> Shapes.texture Material.material.Default
            |> Shapes.transform (scaling 0.5 0.5 0.5)
        let w = Worlds.build_default [s1; s2;]
        (s1,s2,w)

    [<Fact>]
    let ``Constructing a camera``() =
        let c = create_default_camera 160 120
        Assert.Equal(160, c.hsize)
        Assert.Equal(120, c.vsize)
        Assert.Equal(Math.PI / 2.0, c.field_of_view)
        Assert.Equal(identity_matrix (), c.transform)

    [<Fact>]
    let ``The pixel size for a horizontal canvas``() =
        let c = create_default_camera 200 125
        Assert.True(areEqualFloat 0.01 c.pixel_size)

    [<Fact>]
    let ``The pixel size for a vertical canvas``() =
        let c = create_default_camera 125 200
        Assert.True(areEqualFloat 0.01 c.pixel_size)

    [<Fact>]
    let ``Constructing a ray through the center of the canvas``() =
        let c = create_default_camera 201 101
        let r = ray_for_pixel c 100 50
        Assert.Equal(point 0.0 0.0 0.0, roundtuple r.origin)
        Assert.Equal(vector 0.0 0.0 -1.0, roundtuple r.direction)

    [<Fact>]
    let ``Constructing a ray through a corner of the canvas``() =
        let c = create_default_camera 201 101
        let r = ray_for_pixel c 0 0
        Assert.Equal(point 0.0 0.0 0.0, r.origin)
        Assert.Equal(vector 0.66518642611945078 0.33259321305972539 -0.66851235825004807, r.direction)

    [<Fact>]
    let ``Constructing a ray when the camera is transformed``() =
        let c = { create_default_camera 201 101 with transform = rotation_y (Math.PI/4.0) * translation 0.0 -2.0 5.0; }
        let r = ray_for_pixel c 100 50
        Assert.Equal(point 0.0 2.0 -5.0, r.origin)
        Assert.Equal(vector (Math.Sqrt(2.0)/2.0) 0.0 (-Math.Sqrt(2.0)/2.0), r.direction)

    [<Fact>]
    let ``Rendering a world with a camera``() =
        let (_,_,w) = default_world        
        let from_point = point 0.0 0.0 -5.0
        let to_point = point 0.0 0.0 0.0
        let up = vector 0.0 1.0 0.0
        let c = { create_default_camera 11 11 with transform = view_transform from_point to_point up; }
        let image = render c w
        Assert.Equal(color 0.38066119308103435 0.47582649135129296 0.28549589481077575, image.canvas.canvas.[5,5])