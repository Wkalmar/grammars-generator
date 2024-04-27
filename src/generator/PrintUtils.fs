module PrintUtils

open Domain

let formatMidiNote pitch =
    sprintf "%d" pitch.midiNote

let formatDuration pitch =
    sprintf "%f" pitch.duration

let formatArray pitches formatFunction =
    "[" + String.concat ", " (Array.map formatFunction pitches) + "]"

let formatMidiNoteArray pitches =
    formatArray pitches formatMidiNote

let formatDurationArray pitches =
    formatArray pitches formatDuration

let formatDoubleArray (pitches: Pitch array array) =
  let pitchLists =
    pitches
    |> Array.map formatMidiNoteArray

  "[" + String.concat ",\n" pitchLists + "]"
