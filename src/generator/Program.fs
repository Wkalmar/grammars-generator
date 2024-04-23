open System

open Domain
open Utils
open PrintUtils

[<EntryPoint>]
let main argv =
    let items =
        generateProgression HarmonyItem.Tonic 80
        |> Array.map (createChordFromRootNote 64)
        //|> duplicate
        |> duplicateWithReverse
        // |> Array.map (fun chord -> Utils.shuffle chord)
        |> Array.collect (fun chord -> chord)
        // |> Array.iter(fun pitch -> Console.WriteLine("Note: " + pitch.midiNote.ToString() + " Duration: " + pitch.duration.ToString()))
    //let string = formatDoubleArray items
    let string = formatArray items
    Console.ReadLine() |> ignore
    0
