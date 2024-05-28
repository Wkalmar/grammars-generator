open System

open Domain
open Utils
open PrintUtils

[<EntryPoint>]
let main argv =
    let chords =
        generateProgression HarmonyItem.Tonic 80
        |> Array.map (createChordFromRootNote 57)
    let items =
        chords
        //|> duplicate
        |> duplicateWithReverse
        //|> Array.map (fun chord -> Utils.shuffle chord)
        |> Array.collect (fun chord -> chord)
        // |> Array.iter(fun pitch -> Console.WriteLine("Note: " + pitch.midiNote.ToString() + " Duration: " + pitch.duration.ToString()))
    //let string = formatDoubleArray items
    let notes = formatMidiNoteArray items
    let items2 =
        chords
        |> duplicateSixfold
        //|> duplicateWithReverse
        //|> Array.map (fun chord -> Utils.shuffle chord)
        //|> Array.collect (fun chord -> chord)
        // |> Array.iter(fun pitch -> Console.WriteLine("Note: " + pitch.midiNote.ToString() + " Duration: " + pitch.duration.ToString()))
    //let string = formatDoubleArray items
    let notes2 = formatDoubleArray items2
    let durations = formatDurationArray items
    Console.ReadLine() |> ignore
    0
