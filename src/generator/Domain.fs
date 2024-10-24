module Domain

open System

type ChordQuality =
    | Major
    | Minor
    | Major7
    | Dominant7
    | Minor7

type HarmonyItem =
    | Tonic
    | TonicSubstitute1
    | TonicSubstitute2
    | SubDominant
    | SubDominantSubstitute
    | Dominant

type HarmonyItemValue = {
    value: int
    chordQuality: ChordQuality
}

let getHarmonyItemValue item =
    match item with
    | Tonic -> { value = 0; chordQuality = Major7 }
    | TonicSubstitute1 -> { value = 9; chordQuality = Minor7 }
    | TonicSubstitute2 -> { value = 4; chordQuality = Minor7 }
    | SubDominant -> { value = 5; chordQuality = Major7 }
    | SubDominantSubstitute -> { value = 2; chordQuality = Minor7 }
    | Dominant -> { value = 7; chordQuality = Dominant7 }

type HarmonyTransition =
    | Dublicate
    | IncreaseTension
    | IncreaseTensionToSubstitute
    | MaximizeTension
    | DecreaseTension
    | DecreaseTensionToSubstitute
    | Resolve
    | ResolveToFirstSubstitute
    | ResolveToSecondSubstitute

let dublicate harmonyItem =
    harmonyItem

let increaseTension harmonyItem =
    match harmonyItem with
    | Tonic -> SubDominant
    | TonicSubstitute1 -> SubDominant
    | TonicSubstitute2 -> SubDominant
    | SubDominant -> Dominant
    | SubDominantSubstitute -> Dominant
    | Dominant -> Dominant

let increaseTensionToSubstitute harmonyItem =
    match harmonyItem with
    | Tonic -> SubDominantSubstitute
    | TonicSubstitute1 -> SubDominantSubstitute
    | TonicSubstitute2 -> SubDominantSubstitute
    | SubDominant -> Dominant
    | SubDominantSubstitute -> Dominant
    | Dominant -> Dominant

let decreaseTension harmonyItem =
    match harmonyItem with
    | Tonic -> Tonic
    | TonicSubstitute1 -> Tonic
    | TonicSubstitute2 -> Tonic
    | SubDominant -> Tonic
    | SubDominantSubstitute -> Tonic
    | Dominant -> SubDominant

let decreaseTensionToSubstitute harmonyItem =
    match harmonyItem with
    | Tonic -> TonicSubstitute1
    | TonicSubstitute1 -> TonicSubstitute1
    | TonicSubstitute2 -> TonicSubstitute1
    | SubDominant -> TonicSubstitute1
    | SubDominantSubstitute -> TonicSubstitute1
    | Dominant -> SubDominantSubstitute

let maximizeTension harmonyItem =
    Dominant

let resolve harmonyItem =
    Tonic

let resolveToFirstSubstitute harmonyItem =
    TonicSubstitute1

let resolveToSecondSubstitute harmonyItem =
    TonicSubstitute2

type HarmonyTransitionProbability = {
    transition: HarmonyTransition
    coinThreshold: float
}

let regenerateHarmonyTransitionProbability currentHarmonyItem =
    match currentHarmonyItem with
    | Tonic ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
        |]
    | TonicSubstitute1 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
        |]
    | TonicSubstitute2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
        |]
    | SubDominant ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
        |]
    | SubDominantSubstitute ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
        |]
    | Dominant ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.3 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.5 };
            { transition = Resolve; coinThreshold = 0.9 };
            { transition = DecreaseTensionToSubstitute; coinThreshold = 0.95 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
        |]

let applyCommand command chord =
    match command with
    | Dublicate -> dublicate chord
    | IncreaseTension -> increaseTension chord
    | IncreaseTensionToSubstitute -> increaseTensionToSubstitute chord
    | DecreaseTension -> decreaseTension chord
    | DecreaseTensionToSubstitute -> decreaseTensionToSubstitute chord
    | MaximizeTension -> maximizeTension chord
    | Resolve -> resolve chord
    | ResolveToFirstSubstitute -> resolveToFirstSubstitute chord
    | ResolveToSecondSubstitute -> resolveToSecondSubstitute chord

let rnd = Random()

let generateNextChord currentChord coin =
    let probabilityMap = regenerateHarmonyTransitionProbability currentChord
    let command = (Array.filter (fun x -> coin <= x.coinThreshold) probabilityMap).[0].transition
    applyCommand command currentChord

let generateProgression (initialChord: HarmonyItem) (length: int) : HarmonyItem array =
    let rec generate (currentChord: HarmonyItem) (remaining: int) (progression: HarmonyItem list) =
        if remaining = 0 then
            List.toArray (List.rev progression)
        else
            let coin = rnd.NextDouble()
            Console.WriteLine(coin)
            let nextChord = generateNextChord currentChord coin
            generate nextChord (remaining - 1) (nextChord :: progression)
    generate initialChord (length - 1) [initialChord]

type Pitch = {
    midiNote: int
    duration: float
}

let createChordFromRootNote rootNote item =
    let itemValue = getHarmonyItemValue item
    let chord =
        match (itemValue.value, itemValue.chordQuality) with
        | (value, Major) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 4
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 7
                duration = 0.25
            }|]
        | (value, Minor) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 3
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 7
                duration = 0.25
            }|]
        | (value, Dominant7) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 4
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 7
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 10
                duration = 0.25
            }|]
        | (value, Major7) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 4
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 7
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 11
                duration = 0.25
            }|]
        | (value, Minor7) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 3
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 7
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 10
                duration = 0.25
            }|]
    chord
    // match item with
    // | Tonic -> [|
    //     {
    //         midiNote = chord.[0].midiNote
    //         duration = 1.0
    //     };
    //     {
    //         midiNote = chord.[1].midiNote
    //         duration = 0.5
    //     };
    //     {
    //         midiNote = chord.[2].midiNote
    //         duration = 0.5
    //     }|]
    // | TonicSubstitute2 -> [|
    //     {
    //         midiNote = chord.[0].midiNote
    //         duration = 0.5
    //     };
    //     chord.[1];
    //     chord.[2]|]
    // | Dominant -> [|
    //     {
    //         midiNote = chord.[0].midiNote
    //         duration = 0.5
    //     };
    //     chord.[1];
    //     chord.[2]|]
    // | _ -> chord


type OctaveShift = {
    shift: int
    coinThreshold: double
}
let octaveShiftProbabilityMap = [|
            { shift = 0; coinThreshold = 0.5 };
            { shift = 1; coinThreshold = 0.7 };
            { shift = -1; coinThreshold = 0.9 };
            { shift = -2; coinThreshold = 1.0 };
|]

let applyOctaveShift midiNote =
    let coin = rnd.NextDouble()
    let shift =
        (Array.filter (fun x -> coin <= x.coinThreshold) octaveShiftProbabilityMap).[0].shift
    midiNote + (shift * 12)