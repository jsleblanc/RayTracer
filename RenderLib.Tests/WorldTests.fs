namespace RenderLib.Tests

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

    let default_world = { world.Default with objs = [Sphere({ shapeProperties.Default with material = { material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }}); Sphere({ shapeProperties.Default with default_transformation = scaling 0.5 0.5 0.5; })]; }

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
        let w = default_world
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

    [<Fact>]
    let ``Shading an intersection``() =
        let w = default_world
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Seq.item(0) w.objs
        let i = {
            t = 4.0;
            obj = s;
        }
        let comps = prepare_computations i r
        let c = shade_hit w comps
        Assert.Equal(color 0.3806611931 0.4758264914 0.2854958948, c)

    [<Fact>]
    let ``Shading an intersection from the inside``() =
        let w = { default_world with light = { position = point 0.0 0.25 0.0; intensity = color 1.0 1.0 1.0; } }
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Seq.item(1) w.objs
        let i = {
            t = 0.5;
            obj = s;
        }
        let comps = prepare_computations i r
        let c = shade_hit w comps
        Assert.Equal(color 0.9049844721 0.9049844721 0.9049844721, c)

    [<Fact>]
    let ``The color when a ray misses``() =
        let w = default_world
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 1.0 0.0;
        }
        let c = color_at w r
        Assert.Equal(color 0.0 0.0 0.0, c)

    [<Fact>]
    let ``The color when a ray hits``() =
        let w = default_world
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let c = color_at w r
        Assert.Equal(color 0.3806611931 0.4758264914 0.2854958948, c)

    [<Fact>]
    let ``The color with an intersection behind the ray``() =
        let w = { default_world with objs = [Sphere({ shapeProperties.Default with material = { material.Default with ambient = 1.0; }}); Sphere({ shapeProperties.Default with material = { material.Default with ambient = 1.0; }; })]; }
        let inner = Seq.item(1) w.objs
        let r = {
            origin = point 0.0 0.0 0.75;
            direction = vector 0.0 0.0 -1.0;
        }
        let c = color_at w r
        match inner with
        | Sphere s -> Assert.Equal(s.material.color, c)
