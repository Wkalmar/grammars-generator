module PrintUtils

open Domain

let formatPitches (pitches: Pitch array array) =
  let formatPitch (pitch: Pitch) =
    sprintf "%d" pitch.midiNote

  let formatPitchList (pitches: Pitch array) =
    "[" + String.concat ", " (Array.map formatPitch pitches) + "]"

  let pitchLists =
    pitches
    |> Array.map formatPitchList

  "[" + String.concat ",\n" pitchLists + "]"
