module Domain

open System

type ChordQuality =
    | Major
    | Minor

type HarmonyItem =
    | Tonic
    | SubDominant
    | Dominant

type HarmonyItemValue = {
    value: int
    chordQuality: ChordQuality
}

let getHarmonyItemValue item =
    match item with
    | Tonic -> { value = 0; chordQuality = Major }
    | SubDominant -> { value = 5; chordQuality = Major }
    | Dominant -> { value = 7; chordQuality = Major }

type HarmonyTransition =
    | Dublicate
    | IncreaseTension
    | MaximizeTension
    | DecreaseTension
    | Resolve

let dublicate harmonyItem =
    harmonyItem

let increaseTension harmonyItem =
    match harmonyItem with
    | Tonic -> SubDominant
    | SubDominant -> Dominant
    | Dominant -> Dominant

let decreaseTension harmonyItem =
    match harmonyItem with
    | Tonic -> Tonic
    | SubDominant -> Tonic
    | Dominant -> SubDominant

let maximizeTension harmonyItem =
    Dominant

let resolve harmonyItem =
    Tonic

type HarmonyTransitionProbability = {
    transition: HarmonyTransition
    coinThreshold: float
}

let regenerateHarmonyTransitionProbability currentHarmonyItem =
    match currentHarmonyItem with
    | Tonic ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
        |]
    | SubDominant ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = Resolve; coinThreshold = 1.0 };
        |]
    | Dominant ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = Resolve; coinThreshold = 0.55 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
        |]

let applyCommand command chord =
    match command with
    | Dublicate -> dublicate chord
    | IncreaseTension -> increaseTension chord
    | DecreaseTension -> decreaseTension chord
    | MaximizeTension -> maximizeTension chord
    | Resolve -> resolve chord

let rnd = Random()
let roll = rnd.NextDouble()

let generateNextChord currentChord coin =
    let probabilityMap = regenerateHarmonyTransitionProbability currentChord
    let command = (Array.filter (fun x -> coin <= x.coinThreshold) probabilityMap).[0].transition
    applyCommand command currentChord

let generateProgression (initialChord: HarmonyItem) (length: int) : HarmonyItem array =
    let rec generate (currentChord: HarmonyItem) (remaining: int) (progression: HarmonyItem list) =
        if remaining = 0 then
            List.toArray (List.rev progression)
        else
            let coin = roll
            let nextChord = generateNextChord currentChord coin
            generate nextChord (remaining - 1) (nextChord :: progression)
    generate initialChord (length - 1) [initialChord]