namespace RenderLib.Tests

open Xunit
open FsCheck
open RenderLib.Engine

module EngineTests = 
    
    [<Fact>]
    let ``point() creates tuple with w = 1``() =
        let tupleIsPoint x y z = 
            let t = point x y z
            t.w = 1
        Check.QuickThrowOnFailure tupleIsPoint

    [<Fact>]
    let ``vector() creates tuple with w = 0``() =
        let tupleIsVector x y z = 
            let t = vector x y z
            t.w = 0
        Check.QuickThrowOnFailure tupleIsVector
