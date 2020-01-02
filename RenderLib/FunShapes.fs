namespace RenderLib

open System
open Common
open Color
open Tuple
open Matrix
open Ray
open Material
open Shapes
open Translations
open Lights
open Patterns
open Camera
open RenderLib.Color
module FunShapes = 

    let hexagon t = 
        let hexagon_corner =
            ShapeSphere.build
            |> Shapes.transform ((translation 0.0 0.0 -1.0) * (scaling 0.25 0.25 0.25))
        let hexagon_edge =
            ShapeCylinder.build 0.0 1.0 false
            |> Shapes.transform ((translation 0.0 0.0 -1.0) * (rotation_y (-Math.PI/6.0)) * (rotation_z (-Math.PI/2.0)) * (scaling 0.25 1.0 0.25))
        let hexagon_side t =
            ShapeGroup.build [hexagon_corner;hexagon_edge;] |> Shapes.transform t
        let mutable sides = []
        for x in 0 .. 5 do
            let side = hexagon_side (rotation_y (float x * Math.PI / 3.0))
            sides <- side::sides
        ShapeGroup.build sides |> Shapes.transform t
        
    let csg_cube t =
        let plus = 
            let c1 = ShapeCylinder.build -2.0 2.0 true |> Shapes.transform (rotation_z (Common.degrees 90.0) * (scaling 0.5 0.5 0.5)) |> Shapes.texture t 
            let c2 = ShapeCylinder.build -2.0 2.0 true |> Shapes.transform (rotation_y (Common.degrees 90.0) * (scaling 0.5 0.5 0.5)) |> Shapes.texture t 
            let c3 = ShapeCylinder.build -2.0 2.0 true |> Shapes.transform (rotation_x (Common.degrees 90.0) * (scaling 0.5 0.5 0.5)) |> Shapes.texture t 
            let u1 = ShapeCSG.union c1 c2 
            let u2 = ShapeCSG.union u1 c3 
            u2 |> Shapes.transform (scaling 1.1 1.1 1.1) |> Shapes.texture t
        let cube =
            let sphere = ShapeSphere.build |> Shapes.transform (scaling 1.5 1.5 1.5) |> Shapes.texture t 
            let cube = ShapeCube.build |> Shapes.texture t 
            ShapeCSG.intersect cube sphere 
        ShapeCSG.difference cube plus 
            
    let turner_cube t = 
        let cube = 
            let scale = scaling 0.9 0.9 0.9
            let cylinder = ShapeCylinder.build -2.0 2.0 true
            let plus = 
                let c1 = cylinder |> Shapes.transform (rotation_z (Common.degrees 90.0) * (scale)) |> Shapes.texture t 
                let c2 = cylinder |> Shapes.transform (rotation_y (Common.degrees 90.0) * (scale)) |> Shapes.texture t 
                let c3 = cylinder |> Shapes.transform (rotation_x (Common.degrees 90.0) * (scale)) |> Shapes.texture t 
                let u1 = ShapeCSG.union c1 c2 
                ShapeCSG.union u1 c3
            let cube = ShapeCube.build |> Shapes.texture t 
            ShapeCSG.difference cube plus 
        let c1 = cube
        let c2 = cube |> Shapes.transform (scaling 0.75 0.75 0.75)
        let c3 = cube |> Shapes.transform (scaling 0.5 0.5 0.5)
        let c4 = ShapeCube.build |> Shapes.transform (scaling 0.35 0.35 0.35) |> Shapes.texture t
        ShapeGroup.build [c1;c2;c3;c4;]
        
    let sphere_in_sphere_scene =
        let pt = Patterns.checkers (solid_c blue) (solid_c white) |> Patterns.transform (translation 0.0 0.1 0.0)
        let plane = ShapePlane.build |> Shapes.texture { Material.material.Default with pattern = Some pt; } |> Shapes.transform (translation 0.0 -10.1 0.0)
        let s1 = ShapeSphere.build |> Shapes.texture { glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; } |> Shapes.transform (scaling 1.25 1.25 1.25)
        let s2 = ShapeSphere.build |> Shapes.texture { glass with diffuse = 0.1; shininess = 300.0; reflective = 1.0; refractive_index = 1.0;} |> Shapes.transform (scaling 0.75 0.75 0.75)
        let g = ShapeGroup.build [s1;s2;] |> ShapeGroup.divide
        let light = { position = point 20.0 10.0 0.0; intensity = color 0.7 0.7 0.7; }
        let vt = view_transform (point 0.0 2.5 0.0) (point 0.0 0.0 0.0) (vector 1.0 0.0 0.0)
        let camera = { create_default_camera 3840 2160 with field_of_view = Math.PI / 3.0; transform = vt; }
        let world = Worlds.build [plane;g;] light
        render camera world

    let default_world shapes =
        let vt = view_transform (point 0.0 1.5 -3.0) (point 0.0 0.0 0.0) (vector 0.0 1.0 0.0)    
        let camera = { create_default_camera 1024 768 with transform = vt; }
        let light = { position = point 0.0 10.0 -10.0; intensity = color 1.0 1.0 1.0; }
        let world = Worlds.build shapes light
        render camera world