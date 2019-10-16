﻿namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Matrix
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Ray
open RenderLib.Lights
open RenderLib.Worlds

module WorldTests = 

    [<Fact>]
    let ``The default world``() =
        let s1 = Sphere({ shapeProperties.Default with material = { material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }})
        let s2 = Sphere({ shapeProperties.Default with default_transformation = scaling 0.5 0.5 0.5; })
        let w = { world.Default with objs = [s1; s2]; }
        let l = {
            position = point -10.0 10.0 -10.0;
            intensity = color 1.0 1.0 1.0
        }
        Assert.Equal(l, w.light)
        Assert.Contains(s1, w.objs)
        Assert.Contains(s2, w.objs)

    [<Fact>]
    let ``Intersect a world with a ray``() =
        let w = { world.Default with objs = [Sphere({ shapeProperties.Default with material = { material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }}); Sphere({ shapeProperties.Default with default_transformation = scaling 0.5 0.5 0.5; })]; }
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = intersect_world w r
        Assert.Equal(4, Seq.length xs)
        Assert.Equal(4.0, (Seq.item(0) xs).t)
        Assert.Equal(4.5, (Seq.item(1) xs).t)
        Assert.Equal(5.5, (Seq.item(2) xs).t)
        Assert.Equal(6.0, (Seq.item(3) xs).t)
