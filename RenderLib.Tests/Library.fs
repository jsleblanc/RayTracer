namespace RenderLib.Tests

open Xunit

module Say =
    [<Fact>]
    let hello name =
        printfn "Hello %s" name
