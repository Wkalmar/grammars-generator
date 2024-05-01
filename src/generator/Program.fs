open System

open Domain
open Utils
open PrintUtils

[<EntryPoint>]
let main argv =
    let items =
        generateProgression HarmonyItem.Tonic 80
        |> Array.map (createChordFromRootNote 57)
        //|> duplicate
        //|> duplicateWithReverse
        |> Array.map (fun chord -> Utils.shuffle chord)
        |> Array.collect (fun chord -> chord)
        // |> Array.iter(fun pitch -> Console.WriteLine("Note: " + pitch.midiNote.ToString() + " Duration: " + pitch.duration.ToString()))
    //let string = formatDoubleArray items
    let notes = formatMidiNoteArray items
    let durations = formatDurationArray items
    Console.ReadLine() |> ignore
    0
