module PrintUtils

open Domain

let formatMidiNote pitch =
    sprintf "%d" pitch.midiNote

let formatIntNote pitch =
    sprintf "%d" pitch

let formatDuration pitch =
    sprintf "%f" pitch.duration

let formatArray pitches formatFunction =
    "[" + String.concat ", " (Array.map formatFunction pitches) + "]"

let formatMidiNoteArray pitches =
    formatArray pitches formatMidiNote

let formatIntNoteArray notes =
    formatArray notes formatIntNote

let formatDurationArray pitches =
    formatArray pitches formatDuration

let formatDoubleArray (pitches: Pitch array array) =
  let pitchLists =
    pitches
    |> Array.map formatMidiNoteArray

  "[" + String.concat ",\n" pitchLists + "]"
