module PrintUtils

open Domain

let formatPitch (pitch: Pitch) =
    sprintf "%d" pitch.midiNote

let formatArray (pitches: Pitch array) =
    "[" + String.concat ", " (Array.map formatPitch pitches) + "]"

let formatDoubleArray (pitches: Pitch array array) =
  let pitchLists =
    pitches
    |> Array.map formatArray

  "[" + String.concat ",\n" pitchLists + "]"
