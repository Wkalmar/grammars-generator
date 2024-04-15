open System

open Domain
open Utils
open PrintUtils

[<EntryPoint>]
let main argv =
    let items =
        generateProgression HarmonyItem.Tonic 30
        |> Array.map (createChordFromRootNote 64)
        |> duplicate
        // |> Array.map (fun chord -> Utils.shuffle chord)
        // |> Array.collect (fun chord -> chord)
        // |> Array.iter(fun pitch -> Console.WriteLine("Note: " + pitch.midiNote.ToString() + " Duration: " + pitch.duration.ToString()))
    let string = formatPitches items
    Console.ReadLine() |> ignore
    0
