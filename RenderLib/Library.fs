namespace RenderLib

module Engine =
    let hello name =
        printfn "Hello %s" name

    type tuple = {
        x: float;
        y: float;
        z: float;
        w: int;
    }

    let point x y z = { x = x; y = y; z = z; w = 1; }
    let vector x y z = { x = x; y = y; z = z; w = 0; }
