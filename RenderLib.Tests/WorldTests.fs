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

module WorldTests = 

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
    let ``The default world``() =
        let (s1,s2,w) = default_world
        let l = {
            position = point -10.0 10.0 -10.0;
            intensity = color 1.0 1.0 1.0
        }
        Assert.Equal(l, w.light)
        Assert.Contains(s1, w.objs)
        Assert.Contains(s2, w.objs)

    [<Fact>]
    let ``Intersect a world with a ray``() =
        let (s1,s2,w) = default_world
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
        let (s1,s2,w) = default_world
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Seq.item(0) w.objs
        let i = Shapes.build_intersection 4.0 s []
        let comps = prepare i r [i]
        let c = shade_hit w comps 5
        Assert.Equal(color 0.38066119308103435 0.47582649135129296 0.28549589481077575, c)

    [<Fact>]
    let ``Shading an intersection from the inside``() =
        let (s1,s2,w) = default_world
        let w = { w with light = { position = point 0.0 0.25 0.0; intensity = color 1.0 1.0 1.0; } }
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Seq.item(1) w.objs
        let i = Shapes.build_intersection 0.5 s []
        let comps = prepare i r [i]
        let c = shade_hit w comps 5
        Assert.Equal(color 0.9049844721 0.9049844721 0.9049844721, c)

    [<Fact>]
    let ``shade_hit() is given an intersection in shadow``() =
        let s1 = ShapeSphere.build
        let s2 = ShapeSphere.build |> Shapes.transform (translation 0.0 0.0 10.0)
        let w = {
            light = point_light (point 0.0 0.0 -10.0) (color 1.0 1.0 1.0);
            objs = [ s1; s2; ]
        }
        let i = Shapes.build_intersection 4.0 s2 []
        let r = {
            origin = point 0.0 0.0 5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let comps = prepare i r [i]
        let c = shade_hit w comps 5
        Assert.Equal(color 0.1 0.1 0.1, c)

    [<Fact>]
    let ``The color when a ray misses``() =
        let (s1,s2,w) = default_world
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 1.0 0.0;
        }
        let c = color_at w r 5
        Assert.Equal(color 0.0 0.0 0.0, c)

    [<Fact>]
    let ``The color when a ray hits``() =
        let (s1,s2,w) = default_world
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let c = color_at w r 5
        Assert.Equal(color 0.38066119308103435 0.47582649135129296 0.28549589481077575, c)

    [<Fact>]
    let ``The color with an intersection behind the ray``() =
        let (s1,s2,w) = default_world
        let s1 = s1 |> Shapes.texture { s1.material.Value with ambient = 1.0; }
        let s2 = s2 |> Shapes.texture { s2.material.Value with ambient = 1.0; }
        let w = { w with objs = [s1;s2;]; }
        let r = {
            origin = point 0.0 0.0 0.75;
            direction = vector 0.0 0.0 -1.0;
        }
        let c = color_at w r 5
        Assert.Equal(s2.material.Value.color, c)

    [<Fact>]
    let ``There is no shadow when nothing is collinear with point and light``() =
        let (s1,s2,w) = default_world
        let p = point 0.0 10.0 0.0
        let result = is_shadowed w p
        Assert.False(result)

    [<Fact>]
    let ``The shadow when an object is between the point and the light``() =
        let (s1,s2,w) = default_world
        let p = point 10.0 -10.0 10.0
        let result = is_shadowed w p
        Assert.True(result)
        
    [<Fact>]
    let ``There is no shadow when an object is behind the light``() =
        let (s1,s2,w) = default_world
        let p = point -20.0 20.0 -20.0
        let result = is_shadowed w p
        Assert.False(result)

    [<Fact>]
    let ``There is no shadow when an object is behind the point``() =
        let (s1,s2,w) = default_world
        let p = point -2.0 2.0 -2.0
        let result = is_shadowed w p
        Assert.False(result)

    [<Fact>]
    let ``The reflected color for a nonreflective material``() =
        let (s1,s2,w) = default_world
        let s2 = s2 |> Shapes.texture { s2.material.Value with ambient = 1.0; }
        let w = { w with objs = [s1;s2;]; }
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let i = Shapes.build_intersection 1.0 s2 []
        let comps = prepare i r [i]
        let color = reflected_color w comps 5
        Assert.Equal(black, color)

    [<Fact>]
    let ``The reflected color for a reflective material``() = 
        let (s1,s2,w) = default_world
        let p = 
            ShapePlane.build 
            |> Shapes.texture { Material.material.Default with reflective = 0.5; }
            |> Shapes.transform (translation 0.0 -1.0 0.0)
        let w = { w with objs = [s1;s2;p;]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = Shapes.build_intersection (Math.Sqrt(2.0)) p []
        let comps = prepare i r [i]
        let actual = reflected_color w comps 5
        Assert.Equal(color 0.1903305967 0.2379132459 0.1427479475, actual)

    [<Fact>]
    let ``shade_hit() with a reflective material``() =
        let (s1,s2,w) = default_world
        let p = 
            ShapePlane.build 
            |> Shapes.texture { Material.material.Default with reflective = 0.5; }
            |> Shapes.transform (translation 0.0 -1.0 0.0)
        let w = { w with objs = [s1;s2;p;]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = Shapes.build_intersection (Math.Sqrt(2.0)) p []
        let comps = prepare i r [i]
        let c = shade_hit w comps 5
        Assert.Equal(color 0.8767559857 0.9243386349 0.8291733365, c)

    [<Fact>]
    let ``color_at() with mutually reflective surfaces``() =
        let m = { Material.material.Default with reflective = 1.0; }
        let lower = 
            ShapePlane.build
            |> Shapes.texture m
            |> Shapes.transform (translation 0.0 -1.0 0.0)
        let upper = 
            ShapePlane.build
            |> Shapes.texture m
            |> Shapes.transform (translation 0.0 1.0 0.0)
        let w = Worlds.build [lower;upper;] { position = point 0.0 0.0 0.0; intensity = color 1.0 1.0 1.0; }
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 1.0 0.0;
        }
        let c = color_at w r 500
        Assert.NotEqual(black, c)

    [<Fact>]
    let ``The reflected color at the maximum recursive depth``() =
        let (s1,s2,w) = default_world
        let p = 
            ShapePlane.build
            |> Shapes.texture { Material.material.Default with reflective = 0.5; }
            |> Shapes.transform (translation 0.0 -1.0 0.0)        
        let w = { w with objs = [s1;s2;p;]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = Shapes.build_intersection (Math.Sqrt(2.0)) p []
        let comps = prepare i r [i]
        let c = reflected_color w comps 0
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color with an opaque surface``() =
        let (s1,s2,w) = default_world
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = [
            Shapes.build_intersection 4.0 s1 [];
            Shapes.build_intersection 6.0 s1 [];
        ]
        let comps = prepare (Seq.item(0) xs) r xs
        let c = refracted_color w comps 5
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color at the maximum recursive depth``() =
        let (s1,s2,w) = default_world
        let s1 = s1 |> Shapes.texture { glass with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }
        let w = { w with objs = [s1; s2]; }
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = [
            Shapes.build_intersection 4.0 s1 [];
            Shapes.build_intersection 6.0 s1 [];
        ]
        let comps = prepare (Seq.item(0) xs) r xs
        let c = refracted_color w comps 0
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color under total internal reflection``() =
        let (s1,s2,w) = default_world
        let s1 = s1 |> Shapes.texture { glass with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; }
        let w = { w with objs = [s1; s2]; }
        let r = {
            origin = point 0.0 0.0 (Math.Sqrt(2.0)/2.0);
            direction = vector 0.0 1.0 0.0;
        }
        let xs = [
            Shapes.build_intersection (-Math.Sqrt(2.0)/2.0) s1 [];
            Shapes.build_intersection (Math.Sqrt(2.0)/2.0) s1 []; 
        ]
        let comps = prepare (Seq.item(1) xs) r xs
        let c = refracted_color w comps 5
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color with a refracted ray``() =
        let test_pattern =
            let fn point = Patterns.Solid (color point.x point.y point.z)
            Patterns.build fn
        let (s1,s2,w) = default_world
        let s1 = s1 |> Shapes.texture { s1.material.Value with ambient = 1.0; pattern = Some test_pattern; }
        let s2 = s2 |> Shapes.texture glass
        let w = { w with objs = [s1; s2]; }
        let r = {
            origin = point 0.0 0.0 0.1;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = [
            Shapes.build_intersection -0.9899 s1 [];
            Shapes.build_intersection -0.4899 s2 [];
            Shapes.build_intersection 0.4899 s2 [];
            Shapes.build_intersection 0.9899 s1 [];
        ]
        let comps = prepare (Seq.item(2) xs) r xs
        let c = refracted_color w comps 5
        Assert.Equal(color 0.0 0.9988846828 0.04721642191, c)

    [<Fact>]
    let ``shade_hit() with a transparent material``() =
        let (s1,s2,w) = default_world
        let floor = 
            ShapePlane.build
            |> Shapes.texture glass
            |> Shapes.transform (translation 0.0 -1.0 0.0)
        let ball = 
            ShapeSphere.build
            |> Shapes.texture { Material.material.Default with color = color 1.0 0.0 0.0; ambient = 0.25;}
            |> Shapes.transform (translation 0.0 -3.5 -0.5)
        let w = { w with objs = [s1;s2;floor;ball;]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let xs = [
            Shapes.build_intersection (Math.Sqrt(2.0)) floor [];
        ]
        let comps = prepare (Seq.item(0) xs) r xs
        let c = shade_hit w comps 5
        Assert.Equal(color 0.936425389 0.686425389 0.686425389, c)

    [<Fact>]
    let ``shade_hit() with a reflective, transparent material``() =
        let (s1,s2,w) = default_world
        let floor = 
            ShapePlane.build
            |> Shapes.texture { glass with transparency = 0.5; reflective = 0.5; }
            |> Shapes.transform (translation 0.0 -1.0 0.0)
        let ball =
            ShapeSphere.build
            |> Shapes.texture { Material.material.Default with color = color 1.0 0.0 0.0; ambient = 0.5; }
            |> Shapes.transform (translation 0.0 -3.5 -0.5)
        let w = { w with objs = [s1;s2;floor;ball;]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let xs = [
            Shapes.build_intersection (Math.Sqrt(2.0)) floor [];
        ]
        let comps = prepare (Seq.item(0) xs) r xs
        let c = shade_hit w comps 5
        Assert.Equal(color 0.9339151406 0.6964342263 0.6924306914, c)
