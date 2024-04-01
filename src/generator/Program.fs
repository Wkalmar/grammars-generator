open System

open Domain

[<EntryPoint>]
let main argv =
    let items =
        generateProgression HarmonyItem.Tonic 5
        |> Array.map (createChordFromRootNote 60)
        //|> Array.map (fun chord -> Utils.shuffle chord)
        |> Array.collect (fun chord -> chord)
        |> Array.iter(fun pitch -> Console.WriteLine("Note: " + pitch.midiNote.ToString() + " Duration: " + pitch.duration.ToString()))
    Console.ReadLine() |> ignore
    0
