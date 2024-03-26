module Domain

open System

type ChordQuality =
    | Major
    | Minor

type HarmonyItem = {
    value: int
    chordQuality: ChordQuality
}
with
    static member Tonic = { value = 0; chordQuality = Major }
    static member SubDominant = { value = 5; chordQuality = Major }
    static member Dominant = { value = 7; chordQuality = Major }

let createChordFromRootNote rootNote (item: HarmonyItem) =
    match (item.value, item.chordQuality) with
    | (value, Major) -> [rootNote + value; rootNote + value + 4; rootNote + value + 7]
    | (value, Minor) -> [rootNote + value; rootNote + value + 3; rootNote + value + 7]

let rnd = Random()
let roll = rnd.NextDouble()

let generateNextChord currentChord coin =
    match currentChord with
    | Tonic ->
        if coin < 0.1 then HarmonyItem.Tonic
        elif coin < 0.55 then HarmonyItem.SubDominant
        else HarmonyItem.Dominant
    | SubDominant ->
        if coin < 0.5 then HarmonyItem.Tonic
        elif coin < 0.6 then HarmonyItem.SubDominant
        else HarmonyItem.Dominant
    | Dominant ->
        if coin < 0.5 then HarmonyItem.Tonic
        elif coin < 0.9 then HarmonyItem.SubDominant
        else HarmonyItem.Dominant

let generateProgression (initialChord: HarmonyItem) (length: int) : HarmonyItem array =
    let rec generate (currentChord: HarmonyItem) (remaining: int) (progression: HarmonyItem list) =
        if remaining = 0 then
            List.toArray (List.rev progression)
        else
            let coin = roll
            let nextChord = generateNextChord currentChord coin
            generate nextChord (remaining - 1) (nextChord :: progression)
    generate initialChord (length - 1) [initialChord]