namespace RenderLib.Tests

open Xunit
open System
open RenderLib
open RenderLib.Tuple
open RenderLib.Material
open RenderLib.Patterns
open RenderLib.Color
open RenderLib.Lights
open RenderLib.Shapes

module MaterialsTests = 

    let identity x = x

    [<Fact>]
    let ``Lighting with the eye between the light and the surface``() =
        let s = ShapeSphere.build
        let m = Material.material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 0.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m identity light position eyev normalv false
        Assert.Equal(color 1.9 1.9 1.9, r)

    [<Fact>]
    let ``Lighting with the eye between light and surface, eye offset 45 degrees``() =
        let s = ShapeSphere.build
        let m = Material.material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 (Math.Sqrt(2.0)/2.0) (-Math.Sqrt(2.0)/2.0)
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 0.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m identity light position eyev normalv false
        Assert.Equal(color 1.0 1.0 1.0, r)

    [<Fact>]
    let ``Lighting with eye opposite surface, light offset 45 degrees``() =
        let s = ShapeSphere.build
        let m = Material.material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 10.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m identity light position eyev normalv false
        Assert.Equal(color 0.73639610306789272 0.73639610306789272 0.73639610306789272, r)

    [<Fact>]
    let ``Lighting with eye in the path of the reflection vector``() =
        let s = ShapeSphere.build
        let m = Material.material.Default
        let position = point 0.0 0.0 0.0
        let v = -Math.Sqrt(2.0) / 2.0
        let eyev = vector 0.0 v v
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 10.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m identity light position eyev normalv false
        Assert.Equal(color 1.6363961030678928 1.6363961030678928 1.6363961030678928, r)

    [<Fact>]
    let ``Lighting with the light behind the surface``() =
        let s = ShapeSphere.build
        let m = Material.material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 0.0 10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let r = lighting m identity light position eyev normalv false
        Assert.Equal(color 0.1 0.1 0.1, r)

    [<Fact>]
    let ``Lighting with the surface in shadow``() =
        let s = ShapeSphere.build
        let m = Material.material.Default
        let position = point 0.0 0.0 0.0
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = {
            position = point 0.0 0.0 -10.0;
            intensity = color 1.0 1.0 1.0;
        } 
        let in_shadow = true
        let result = lighting m identity light position eyev normalv in_shadow
        Assert.Equal(color 0.1 0.1 0.1, result)

    [<Fact>]
    let ``Lighting with a pattern applied``() =
        let s = ShapeSphere.build
        let pt = Patterns.stripe (solid_c white) (solid_c black)
        let m = { Material.material.Default with pattern = Some pt; ambient = 1.0; diffuse = 0.0; specular = 0.0; }
        let eyev = vector 0.0 0.0 -1.0
        let normalv = vector 0.0 0.0 -1.0
        let light = point_light (point 0.0 0.0 -10.0) white
        let c1 = lighting m identity light (point 0.9 0.0 0.0) eyev normalv false
        let c2 = lighting m identity light (point 1.1 0.0 0.0) eyev normalv false
        Assert.Equal(white, c1)
        Assert.Equal(black, c2)
