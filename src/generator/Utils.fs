module Utils

let shuffle d =
    let rnd = System.Random ()
    d |> Array.sortBy(fun _ -> rnd.Next(1, 52) )

let duplicate (arr) =
    arr
    |> Array.map (fun x -> [|x; x|])
    |> Array.concat

let duplicateSixfold (arr) =
    arr
    |> Array.map (fun x -> [|x; x; x; x; x; x;|])
    |> Array.concat

let duplicateWithReverse (arr) =
    arr
    |> Array.map (fun x -> [|x; Array.rev x|])
    |> Array.concat
