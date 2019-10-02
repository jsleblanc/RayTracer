namespace RenderLib

open Matrix

module Translations =

    let translation x y z =
        let i = identity_matrix ()
        i.[0,3] <- x
        i.[1,3] <- y
        i.[2,3] <- z
        i