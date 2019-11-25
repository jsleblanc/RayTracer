namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Material
open RenderLib.Shapes
open RenderLib.PreparedComputations
open RenderLib.Translations
open RenderLib.Ray
open RenderLib.Lights
open RenderLib.Worlds
open RenderLib.Patterns
open RenderLib.Matrix
open RenderLib.Camera
open RenderLib.Scenes

module SceneTests = 

    [<Fact>]
    let ``Should add camera to scene``() =
        let yaml = 
            "
            - add: camera
              width: 100
              height: 100
              field-of-view: 0.785
              from: [ -6, 6, -10 ]
              to: [ 6, 0, 6 ]
              up: [ -0.45, 1, 0 ]
            "
        let vt = Translations.view_transform (point -6.0 6.0 -10.0) (point 6.0 0.0 6.0) (vector -0.45 1.0 0.0)
        let expected = { (Camera.create_camera 100 100 0.785) with transform = vt; }
        let scene = Scenes.parse_text yaml
        Assert.Equal(expected, scene.camera)

    [<Fact>]
    let ``Should add light``() =
        let yaml =
            "
            - add: light
              at: [ 50, 100, -50 ]
              intensity: [ 1, 1, 1 ]
            "
        let expected = {
            position = point 50.0 100.0 -50.0;
            intensity = color 1.0 1.0 1.0;
        }
        let scene = Scenes.parse_text yaml
        Assert.True(List.contains expected scene.lights)

    [<Fact>]
    let ``Should define material``() =
        let yaml = 
            "
            - define: white-material
              value:
                color: [ 1, 1, 1 ]
                diffuse: 0.7
                ambient: 0.1
                specular: 0.0
                reflective: 0.1
              "
        let expected = { Material.material.Default with color = color 1.0 1.0 1.0; diffuse = 0.7; ambient = 0.1; specular = 0.0; reflective = 0.1; }
        let scene = Scenes.parse_text yaml        
        Assert.Equal(expected, scene.materials.["white-material"])

    [<Fact>]
    let ``Should define material that extends existing material``() =
        let yaml =
            "
            - define: white-material
              value:
                color: [ 1, 1, 1 ]
                diffuse: 0.7
                ambient: 0.1
                specular: 0.0
                reflective: 0.1

            - define: blue-material
              extend: white-material
              value:
                color: [ 0.537, 0.831, 0.914 ]
            "
        let expected = { Material.material.Default with color = color 0.537 0.831 0.914; diffuse = 0.7; ambient = 0.1; specular = 0.0; reflective = 0.1; }
        let scene = Scenes.parse_text yaml        
        Assert.Equal(expected, scene.materials.["blue-material"])

    [<Fact>]
    let ``Should define transformation``() =
        let yaml =
            "
            - define: standard-transform
              value:
                - [ translate, 1, -1, 1 ]
                - [ scale, 0.5, 0.5, 0.5 ]
            "
        let expected = [Translation(0.5,0.5,0.5);Scaling(0.5,0.5,0.5);]
        let scene = Scenes.parse_text yaml
        let result = expected = scene.transformations.["standard-transform"]
        Assert.True(result)

    [<Fact>]
    let ``Should define transformation that extends existing transformation``() =
        let yaml =
            "
            - define: large-object
              value:
                - standard-transform
                - [ scale, 3.5, 3.5, 3.5 ]
            "
        let expected = [Translation(0.5,0.5,0.5);Scaling(0.5,0.5,0.5);Scaling(3.5,3.5,3.5);]
        let scene = Scenes.parse_text yaml
        let result = expected = scene.transformations.["large-object"]
        Assert.True(result)

    [<Fact>]
    let ``Should add plane to scene``() =
        let yaml = 
            "
            - add: plane
              material:
                color: [ 1, 1, 1 ]
                ambient: 1
                diffuse: 0
                specular: 0
              transform:
                - [ rotate-x, 1.5707963267948966 ] # pi/2
                - [ translate, 0, 0, 500 ]
            "
        let expected = {
            //shape = Plane;
            material = Some { Material.material.Default with color = color 1.0 1.0 1.0; ambient = 1.0; diffuse = 0.0; specular = 0.0; };
            shadow = false;
            shapeObj = "";
            transformations = [Rotation_X(Math.PI/2.0);Translation(0.0,0.0,500.0);]
        }
        let scene = Scenes.parse_text yaml
        Assert.True(List.contains expected scene.shapes)

    [<Fact>]
    let ``Should add shape template to scene``() =
        let yaml = 
            "
            - define: raw-bbox
              value:
                add: cube
                shadow: false
                transform:
                  - [ translate, 1, 1, 1 ]
                  - [ scale, 3.73335, 2.5845, 1.6283 ]
                  - [ translate, -3.9863, -0.1217, -1.1820 ]
            "
        let expected = {
            //shape = Cube;
            material = Some { Material.material.Default with color = color 1.0 1.0 1.0; ambient = 1.0; diffuse = 0.0; specular = 0.0; };
            shadow = false;
            shapeObj = "";
            transformations = [Translation(1.0,1.0,1.0);Scaling(3.73335,2.5848,1.6283);Translation(-3.9863,-0.1217,-1.1820);]
        }
        let scene = Scenes.parse_text yaml
        Assert.Equal(expected, scene.shape_templates.["raw-bbox"])