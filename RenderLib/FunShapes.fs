namespace RenderLib

open System
open Common
open Tuple
open Matrix
open Ray
open Material
open Shapes
open Translations

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
        
    let csg_test =
        let plus = 
            let c1 = ShapeCylinder.build -2.0 2.0 true |> Shapes.transform ((scaling 0.5 0.5 1.0) * rotation_z (Common.degrees 90.0))
            let c2 = ShapeCylinder.build -2.0 2.0 true |> Shapes.transform ((scaling 0.5 0.5 1.0) * rotation_y (Common.degrees 90.0))
            let c3 = ShapeCylinder.build -2.0 2.0 true |> Shapes.transform ((scaling 0.5 0.5 1.0) * rotation_x (Common.degrees 90.0))
            let u = ShapeCSG.union c1 c2
            ShapeCSG.union u c3
        let cube =
            let s1 = ShapeSphere.build |> Shapes.transform (scaling 0.75 0.75 0.75)
            let s2 = ShapeCube.build |> Shapes.transform (scaling 1.25 1.25 1.25)
            ShapeCSG.intersect s1 s2
        //ShapeCSG.difference cube plus
        cube
            
