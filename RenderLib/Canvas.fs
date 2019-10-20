namespace RenderLib

open Color

module Canvas = 

    type canvas = color[,]

    let create_canvas x y : canvas =
        let black = color 0.0 0.0 0.0
        Array2D.create x y black

    let write_pixel x y color (cv:canvas) =
        cv.[x,y] <- color
        cv