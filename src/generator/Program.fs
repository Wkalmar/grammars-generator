open System

type ChordQuality =
    | Major
    | Minor

type HarmonyItem = {
    value: int
    chordQuality: ChordQuality
}
with
    static member Tonic = { value = 1; chordQuality = Major }
    static member SubDominant = { value = 4; chordQuality = Major }
    static member Dominant = { value = 5; chordQuality = Major }

let createChordFromRootNote rootNote (item: HarmonyItem) =
    match (item.value, item.chordQuality) with
    | (value, Major) -> [rootNote + value-1; rootNote + value-1 + 4; rootNote + value-1 + 7]
    | (value, Minor) -> [rootNote + value-1; rootNote + value-1 + 3; rootNote + value-1 + 7]

let generateNextChord (currentChord: HarmonyItem) : HarmonyItem =
    let rnd = System.Random()
    let roll = rnd.NextDouble()
    match currentChord with
    | Tonic ->
        if roll < 0.1 then Tonic
        elif roll < 0.55 then HarmonyItem.SubDominant
        else HarmonyItem.Dominant
    | SubDominant ->
        if roll < 0.5 then HarmonyItem.Tonic
        elif roll < 0.6 then SubDominant
        else HarmonyItem.Dominant
    | Dominant ->
        if roll < 0.5 then HarmonyItem.Tonic
        elif roll < 0.9 then HarmonyItem.SubDominant
        else Dominant

let generateProgression (initialChord: HarmonyItem) (length: int) : HarmonyItem array =
    let rec generate (currentChord: HarmonyItem) (remaining: int) (progression: HarmonyItem list) =
        if remaining = 0 then
            List.toArray (List.rev progression)
        else
            let nextChord = generateNextChord currentChord
            generate nextChord (remaining - 1) (nextChord :: progression)
    generate initialChord (length - 1) [initialChord]

[<EntryPoint>]
let main argv =
    let items =
        generateProgression HarmonyItem.Tonic 10
        |> Array.map (createChordFromRootNote 60)
    Console.ReadLine() |> ignore
    0
