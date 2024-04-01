module Utils

let shuffle d =
    let rnd = System.Random ()
    d |> Array.sortBy(fun _ -> rnd.Next(1, 52) )
