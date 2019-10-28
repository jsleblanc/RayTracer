namespace RenderLib.Tests

open Xunit
open FsCheck
open System
open RenderLib.Common
open RenderLib.Tuple
open RenderLib.Color
open RenderLib.Material
open RenderLib.Shapes
open RenderLib.Translations
open RenderLib.Ray
open RenderLib.Lights
open RenderLib.Worlds
open RenderLib.Patterns
open RenderLib.Matrix

module WorldTests = 

    [<Fact>]
    let ``The default world``() =
        let s1 = Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix())
        let s2 = Sphere(material.Default,scaling 0.5 0.5 0.5)
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
        let w = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
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
        let w = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let s = Seq.item(0) w.objs
        let i = {
            t = 4.0;
            obj = s;
        }
        let comps = prepare_computations i r (seq {i})
        let c = shade_hit w comps 5
        Assert.Equal(color 0.38066119308103435 0.47582649135129296 0.28549589481077575, c)

    [<Fact>]
    let ``Shading an intersection from the inside``() =
        let default_world = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
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
        let comps = prepare_computations i r (seq {i})
        let c = shade_hit w comps 5
        Assert.Equal(color 0.9049844721 0.9049844721 0.9049844721, c)

    [<Fact>]
    let ``shade_hit() is given an intersection in shadow``() =
        let s1 = Sphere(material.Default,identity_matrix())
        let s2 = Sphere(material.Default,translation 0.0 0.0 10.0)
        let w = {
            light = point_light (point 0.0 0.0 -10.0) (color 1.0 1.0 1.0);
            objs = [ s1; s2; ]
        }
        let i = {
            t = 4.0;
            obj = s2;
        }
        let r = {
            origin = point 0.0 0.0 5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let comps = prepare_computations i r (seq {i})
        let c = shade_hit w comps 5
        Assert.Equal(color 0.1 0.1 0.1, c)

    [<Fact>]
    let ``The color when a ray misses``() =
        let w = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 1.0 0.0;
        }
        let c = color_at w r 5
        Assert.Equal(color 0.0 0.0 0.0, c)

    [<Fact>]
    let ``The color when a ray hits``() =
        let w = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let c = color_at w r 5
        Assert.Equal(color 0.38066119308103435 0.47582649135129296 0.28549589481077575, c)

    [<Fact>]
    let ``The color with an intersection behind the ray``() =
        let default_world = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let w = { default_world with objs = [Sphere({ material.Default with ambient = 1.0; },identity_matrix()); Sphere({ material.Default with ambient = 1.0; },identity_matrix())]; }
        let inner = Seq.item(1) w.objs
        let r = {
            origin = point 0.0 0.0 0.75;
            direction = vector 0.0 0.0 -1.0;
        }
        let c = color_at w r 5
        match inner with
        | Sphere (m,t) -> Assert.Equal(m.color, c)

    [<Fact>]
    let ``There is no shadow when nothing is collinear with point and light``() =
        let default_world = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let p = point 0.0 10.0 0.0
        let result = is_shadowed default_world p
        Assert.False(result)

    [<Fact>]
    let ``The shadow when an object is between the point and the light``() =
        let default_world = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let p = point 10.0 -10.0 10.0
        let result = is_shadowed default_world p
        Assert.True(result)
        
    [<Fact>]
    let ``There is no shadow when an object is behind the light``() =
        let default_world = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let p = point -20.0 20.0 -20.0
        let result = is_shadowed default_world p
        Assert.False(result)

    [<Fact>]
    let ``There is no shadow when an object is behind the point``() =
        let default_world = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5)]; }
        let p = point -2.0 2.0 -2.0
        let result = is_shadowed default_world p
        Assert.False(result)

    [<Fact>]
    let ``The reflected color for a nonreflective material``() =
        let s2 = Sphere({ material.Default with ambient = 1.0; },scaling 0.5 0.5 0.5)
        let default_world = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); s2]; }
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 0.0 1.0;
        }
        let i = {
            t = 1.0;
            obj = s2;
        }
        let comps = prepare_computations i r (seq {i})
        let color = reflected_color default_world comps 5
        Assert.Equal(black, color)

    [<Fact>]
    let ``The reflected color for a reflective material``() = 
        let p = Plane({ material.Default with reflective = 0.5; }, translation 0.0 -1.0 0.0)
        let w = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5); p; ]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = {
            t = Math.Sqrt(2.0);
            obj = p;
        }
        let comps = prepare_computations i r (seq {i})
        let actual = reflected_color w comps 5
        Assert.Equal(color 0.1903305967 0.2379132459 0.1427479475, actual)

    [<Fact>]
    let ``shade_hit() with a reflective material``() =
        let p = Plane({ material.Default with reflective = 0.5; }, translation 0.0 -1.0 0.0)
        let w = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); p; ]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = {
            t = Math.Sqrt(2.0);
            obj = p;
        }
        let comps = prepare_computations i r (seq {i})
        let c = shade_hit w comps 5
        Assert.Equal(color 0.8767559857 0.9243386349 0.8291733365, c)

    [<Fact>]
    let ``color_at() with mutually reflective surfaces``() =
        let lower = Plane({ material.Default with reflective = 1.0; },translation 0.0 -1.0 0.0)
        let upper = Plane({ material.Default with reflective = 1.0; },translation 0.0 1.0 0.0)
        let w = { world.Default with objs = [ lower; upper; ] }
        let r = {
            origin = point 0.0 0.0 0.0;
            direction = vector 0.0 1.0 0.0;
        }
        let c = color_at w r 500
        Assert.NotEqual(black, c)

    [<Fact>]
    let ``The reflected color at the maximum recursive depth``() =
        let p = Plane({ material.Default with reflective = 0.5; },translation 0.0 -1.0 0.0)
        let w = { world.Default with objs = [Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); p; ]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let i = {
            t = Math.Sqrt(2.0);
            obj = p;
        }
        let comps = prepare_computations i r (seq {i})
        let c = reflected_color w comps 0
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color with an opaque surface``() =
        let s1 = Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix())
        let s2 = Sphere(material.Default,scaling 0.5 0.5 0.5)
        let w = { world.Default with objs = [s1; s2]; }
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = seq {
            { t = 4.0; obj = s1; }
            { t = 6.0; obj = s1; }
        }
        let comps = prepare_computations (Seq.item(0) xs) r xs
        let c = refracted_color w comps 5
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color at the maximum recursive depth``() =
        let s1 = Sphere({ glass with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix())
        let s2 = Sphere(material.Default,scaling 0.5 0.5 0.5)
        let w = { world.Default with objs = [s1; s2]; }
        let r = {
            origin = point 0.0 0.0 -5.0;
            direction = vector 0.0 0.0 1.0;
        }
        let xs = seq {
            { t = 4.0; obj = s1; }
            { t = 6.0; obj = s1; }
        }
        let comps = prepare_computations (Seq.item(0) xs) r xs
        let c = refracted_color w comps 0
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color under total internal reflection``() =
        let s1 = Sphere({ glass with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix())
        let s2 = Sphere(material.Default,scaling 0.5 0.5 0.5)
        let w = { world.Default with objs = [s1; s2]; }
        let r = {
            origin = point 0.0 0.0 (Math.Sqrt(2.0)/2.0);
            direction = vector 0.0 1.0 0.0;
        }
        let xs = seq {
            { t = (-Math.Sqrt(2.0)/2.0); obj = s1; }
            { t = (Math.Sqrt(2.0)/2.0); obj = s1; }
        }
        let comps = prepare_computations (Seq.item(1) xs) r xs
        let c = refracted_color w comps 5
        Assert.Equal(black, c)

    [<Fact>]
    let ``The refracted color with a refracted ray``() =
        let s1 = Sphere({ material.Default with ambient = 1.0; pattern = Some pattern.Test; },identity_matrix())
        let s2 = Sphere(glass,scaling 0.5 0.5 0.5)
        let w = { world.Default with objs = [s1; s2]; }
        let r = {
            origin = point 0.0 0.0 0.1;
            direction = vector 0.0 1.0 0.0;
        }
        let xs = seq {
            { t = -0.9899; obj = s1; }
            { t = -0.4899; obj = s2; }
            { t = 0.4899; obj = s2; }
            { t = 0.9899; obj = s1; }
        }
        let comps = prepare_computations (Seq.item(2) xs) r xs
        let c = refracted_color w comps 5
        Assert.Equal(color 0.0 0.9988846828 0.04721642191, c)

    [<Fact>]
    let ``shade_hit() with a transparent material``() =
        let floor = Plane(glass,translation 0.0 -1.0 0.0)
        let ball = Sphere({ material.Default with color = color 1.0 0.0 0.0; ambient = 0.25;},translation 0.0 -3.5 -0.5)
        let w = { world.Default with objs = [ Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5); floor; ball; ]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let xs = seq {
            { t = Math.Sqrt(2.0); obj = floor; }
        }
        let comps = prepare_computations (Seq.item(0) xs) r xs
        let c = shade_hit w comps 5
        Assert.Equal(color 0.936425389 0.686425389 0.686425389, c)

    [<Fact>]
    let ``shade_hit() with a reflective, transparent material``() =
        let floor = Plane({ glass with transparency = 0.5; reflective = 0.5; },translation 0.0 -1.0 0.0)
        let ball = Sphere({ material.Default with color = color 1.0 0.0 0.0; ambient = 0.5; },translation 0.0 -3.5 -0.5)
        let w = { world.Default with objs = [ Sphere({ material.Default with color = color 0.8 1.0 0.6; diffuse = 0.7; specular = 0.2; },identity_matrix()); Sphere(material.Default,scaling 0.5 0.5 0.5); floor; ball; ]; }
        let r = {
            origin = point 0.0 0.0 -3.0;
            direction = vector 0.0 (-Math.Sqrt(2.0)/2.0) (Math.Sqrt(2.0)/2.0);
        }
        let xs = seq {
            { t = Math.Sqrt(2.0); obj = floor; };
        }
        let comps = prepare_computations (Seq.item(0) xs) r xs
        let c = shade_hit w comps 5
        Assert.Equal(color 0.9339151406 0.6964342263 0.6924306914, c)
