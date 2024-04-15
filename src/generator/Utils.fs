module Utils

let shuffle d =
    let rnd = System.Random ()
    d |> Array.sortBy(fun _ -> rnd.Next(1, 52) )

let duplicate (arr: 'a []) =
    arr
    |> Array.map (fun x -> [|x; x|])
    |> Array.concat
