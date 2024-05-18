module Domain

open System

type ChordQuality =
    | Major
    | Minor
    | Diminished
    | Suspended2
    | Suspended4

type HarmonyItem =
    | Tonic
    | TonicSuspended2
    | TonicSuspended4
    | TonicSubstitute1
    | TonicSubstitute1Suspended2
    | TonicSubstitute1Suspended4
    | TonicSubstitute2
    | TonicSubstitute2Suspended2
    | TonicSubstitute2Suspended4
    | SubDominant
    | SubDominantSuspended2
    | SubDominantSuspended4
    | SubDominantSubstitute
    | SubDominantSubstituteSuspended2
    | SubDominantSubstituteSuspended4
    | Dominant
    | DominantSuspended2
    | DominantSuspended4
    | DominantSubstitute
    | DominantSubstituteSuspended2
    | DominantSubstituteSuspended4

type HarmonyItemValue = {
    value: int
    chordQuality: ChordQuality
}

let getHarmonyItemValue item =
    match item with
    | Tonic -> { value = 0; chordQuality = Major }
    | TonicSuspended2 -> { value = 0; chordQuality = Suspended2 }
    | TonicSuspended4 -> { value = 0; chordQuality = Suspended4 }
    | TonicSubstitute1 -> { value = 9; chordQuality = Minor }
    | TonicSubstitute1Suspended2 -> { value = 9; chordQuality = Suspended2 }
    | TonicSubstitute1Suspended4 -> { value = 9; chordQuality = Suspended4 }
    | TonicSubstitute2 -> { value = 4; chordQuality = Minor }
    | TonicSubstitute2Suspended2 -> { value = 4; chordQuality = Suspended2 }
    | TonicSubstitute2Suspended4 -> { value = 4; chordQuality = Suspended4 }
    | SubDominant -> { value = 5; chordQuality = Major }
    | SubDominantSuspended2 -> { value = 5; chordQuality = Suspended2 }
    | SubDominantSuspended4 -> { value = 5; chordQuality = Suspended4 }
    | SubDominantSubstitute -> { value = 2; chordQuality = Minor }
    | SubDominantSubstituteSuspended2 -> { value = 2; chordQuality = Suspended2 }
    | SubDominantSubstituteSuspended4 -> { value = 2; chordQuality = Suspended4 }
    | Dominant -> { value = 7; chordQuality = Major }
    | DominantSuspended2 -> { value = 7; chordQuality = Suspended2 }
    | DominantSuspended4 -> { value = 7; chordQuality = Suspended4 }
    | DominantSubstitute -> { value = 11; chordQuality = Diminished }
    | DominantSubstituteSuspended2 -> { value = 11; chordQuality = Suspended2 }
    | DominantSubstituteSuspended4 -> { value = 11; chordQuality = Suspended4 }

type HarmonyTransition =
    | Dublicate
    | IncreaseTension
    | IncreaseTensionToSubstitute
    | MaximizeTension
    | MaximizeTensionToSubstitute
    | DecreaseTension
    | DecreaseTensionToSubstitute
    | Resolve
    | ResolveToFirstSubstitute
    | ResolveToSecondSubstitute
    | AddColor2
    | AddColor4

let dublicate harmonyItem =
    harmonyItem

let increaseTension harmonyItem =
    match harmonyItem with
    | Tonic -> SubDominant
    | TonicSuspended2 -> SubDominant
    | TonicSuspended4 -> SubDominant
    | TonicSubstitute1 -> SubDominant
    | TonicSubstitute1Suspended2 -> SubDominant
    | TonicSubstitute1Suspended4 -> SubDominant
    | TonicSubstitute2 -> SubDominant
    | TonicSubstitute2Suspended2 -> SubDominant
    | TonicSubstitute2Suspended4 -> SubDominant
    | SubDominant -> Dominant
    | SubDominantSuspended2 -> Dominant
    | SubDominantSuspended4 -> Dominant
    | SubDominantSubstitute -> Dominant
    | SubDominantSubstituteSuspended2 -> Dominant
    | SubDominantSubstituteSuspended4 -> Dominant
    | Dominant -> Dominant
    | DominantSuspended2 -> Dominant
    | DominantSuspended4 -> Dominant
    | DominantSubstitute -> Dominant
    | DominantSubstituteSuspended2 -> Dominant
    | DominantSubstituteSuspended4 -> Dominant

let increaseTensionToSubstitute harmonyItem =
    match harmonyItem with
    | Tonic -> SubDominantSubstitute
    | TonicSuspended2 -> SubDominantSubstitute
    | TonicSuspended4 -> SubDominantSubstitute
    | TonicSubstitute1 -> SubDominantSubstitute
    | TonicSubstitute1Suspended2 -> SubDominantSubstitute
    | TonicSubstitute1Suspended4 -> SubDominantSubstitute
    | TonicSubstitute2 -> SubDominantSubstitute
    | TonicSubstitute2Suspended2 -> SubDominantSubstitute
    | TonicSubstitute2Suspended4 -> SubDominantSubstitute
    | SubDominant -> DominantSubstitute
    | SubDominantSuspended2 -> DominantSubstitute
    | SubDominantSuspended4 -> DominantSubstitute
    | SubDominantSubstitute -> DominantSubstitute
    | SubDominantSubstituteSuspended2 -> DominantSubstitute
    | SubDominantSubstituteSuspended4 -> DominantSubstitute
    | Dominant -> DominantSubstitute
    | DominantSuspended2 -> DominantSubstitute
    | DominantSuspended4 -> DominantSubstitute
    | DominantSubstitute -> DominantSubstitute
    | DominantSubstituteSuspended2 -> DominantSubstitute
    | DominantSubstituteSuspended4 -> DominantSubstitute


let decreaseTension harmonyItem =
    match harmonyItem with
    | Tonic -> Tonic
    | TonicSuspended2 -> Tonic
    | TonicSuspended4 -> Tonic
    | TonicSubstitute1 -> Tonic
    | TonicSubstitute1Suspended2 -> Tonic
    | TonicSubstitute1Suspended4 -> Tonic
    | TonicSubstitute2 -> Tonic
    | TonicSubstitute2Suspended2 -> Tonic
    | TonicSubstitute2Suspended4 -> Tonic
    | SubDominant -> Tonic
    | SubDominantSuspended2 -> Tonic
    | SubDominantSuspended4 -> Tonic
    | SubDominantSubstitute -> Tonic
    | SubDominantSubstituteSuspended2 -> Tonic
    | SubDominantSubstituteSuspended4 -> Tonic
    | Dominant -> SubDominant
    | DominantSuspended2 -> SubDominant
    | DominantSuspended4 -> SubDominant
    | DominantSubstitute -> SubDominant
    | DominantSubstituteSuspended2 -> SubDominant
    | DominantSubstituteSuspended4 -> SubDominant

let decreaseTensionToSubstitute harmonyItem =
    match harmonyItem with
    | Tonic -> TonicSubstitute1
    | TonicSuspended2 -> TonicSubstitute1
    | TonicSuspended4 -> TonicSubstitute1
    | TonicSubstitute1 -> TonicSubstitute1
    | TonicSubstitute1Suspended2 -> TonicSubstitute1
    | TonicSubstitute1Suspended4 -> TonicSubstitute1
    | TonicSubstitute2 -> TonicSubstitute1
    | TonicSubstitute2Suspended2 -> TonicSubstitute1
    | TonicSubstitute2Suspended4 -> TonicSubstitute1
    | SubDominant -> TonicSubstitute1
    | SubDominantSuspended2 -> TonicSubstitute1
    | SubDominantSuspended4 -> TonicSubstitute1
    | SubDominantSubstitute -> TonicSubstitute1
    | SubDominantSubstituteSuspended2 -> TonicSubstitute1
    | SubDominantSubstituteSuspended4 -> TonicSubstitute1
    | Dominant -> SubDominantSubstitute
    | DominantSuspended2 -> SubDominantSubstitute
    | DominantSuspended4 -> SubDominantSubstitute
    | DominantSubstitute -> SubDominantSubstitute
    | DominantSubstituteSuspended2 -> SubDominantSubstitute
    | DominantSubstituteSuspended4 -> SubDominantSubstitute

let maximizeTension harmonyItem =
    Dominant

let maximizeTensionToSubstitute harmonyItem =
    DominantSubstitute

let resolve harmonyItem =
    Tonic

let resolveToFirstSubstitute harmonyItem =
    TonicSubstitute1

let resolveToSecondSubstitute harmonyItem =
    TonicSubstitute2

let addColor2 harmonyItem =
    match harmonyItem with
    | Tonic -> TonicSuspended2
    | TonicSuspended2 -> Tonic
    | TonicSuspended4 -> TonicSuspended2
    | TonicSubstitute1 -> TonicSubstitute1Suspended2
    | TonicSubstitute1Suspended2 -> TonicSubstitute1
    | TonicSubstitute1Suspended4 -> TonicSubstitute1Suspended2
    | TonicSubstitute2 -> TonicSubstitute2Suspended2
    | TonicSubstitute2Suspended2 -> TonicSubstitute2
    | TonicSubstitute2Suspended4 -> TonicSubstitute2Suspended2
    | SubDominant -> SubDominantSuspended2
    | SubDominantSuspended2 -> SubDominant
    | SubDominantSuspended4 -> SubDominantSuspended2
    | SubDominantSubstitute -> SubDominantSubstituteSuspended2
    | SubDominantSubstituteSuspended2 -> SubDominantSubstitute
    | SubDominantSubstituteSuspended4 -> SubDominantSubstituteSuspended2
    | Dominant -> DominantSuspended2
    | DominantSuspended2 -> Dominant
    | DominantSuspended4 -> DominantSuspended2
    | DominantSubstitute -> DominantSubstituteSuspended2
    | DominantSubstituteSuspended2 -> DominantSubstitute
    | DominantSubstituteSuspended4 -> DominantSubstituteSuspended2

let addColor4 harmonyItem =
    match harmonyItem with
    | Tonic -> TonicSuspended4
    | TonicSuspended2 -> TonicSuspended4
    | TonicSuspended4 -> Tonic
    | TonicSubstitute1 -> TonicSubstitute1Suspended4
    | TonicSubstitute1Suspended2 -> TonicSubstitute1Suspended4
    | TonicSubstitute1Suspended4 -> TonicSubstitute1
    | TonicSubstitute2 -> TonicSubstitute2Suspended4
    | TonicSubstitute2Suspended2 -> TonicSubstitute2Suspended4
    | TonicSubstitute2Suspended4 -> TonicSubstitute2
    | SubDominant -> SubDominantSuspended4
    | SubDominantSuspended2 -> SubDominantSuspended4
    | SubDominantSuspended4 -> SubDominant
    | SubDominantSubstitute -> SubDominantSubstituteSuspended4
    | SubDominantSubstituteSuspended2 -> SubDominantSubstituteSuspended4
    | SubDominantSubstituteSuspended4 -> SubDominantSubstitute
    | Dominant -> DominantSuspended4
    | DominantSuspended2 -> DominantSuspended4
    | DominantSuspended4 -> Dominant
    | DominantSubstitute -> DominantSubstituteSuspended4
    | DominantSubstituteSuspended2 -> DominantSubstituteSuspended2
    | DominantSubstituteSuspended4 -> DominantSubstitute

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
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSuspended2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSuspended4 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSubstitute1 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSubstitute1Suspended2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSubstitute1Suspended4 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSubstitute2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSubstitute2Suspended2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | TonicSubstitute2Suspended4 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = MaximizeTension; coinThreshold = 0.75 };
            { transition = MaximizeTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | SubDominant ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | SubDominantSuspended2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | SubDominantSuspended4 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | SubDominantSubstitute ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | SubDominantSubstituteSuspended2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | SubDominantSubstituteSuspended4 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = IncreaseTensionToSubstitute; coinThreshold = 0.3 };
            { transition = IncreaseTension; coinThreshold = 0.55 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.65 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.75 };
            { transition = Resolve; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | Dominant ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.3 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.5 };
            { transition = Resolve; coinThreshold = 0.9 };
            { transition = DecreaseTensionToSubstitute; coinThreshold = 0.95 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | DominantSuspended2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.3 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.5 };
            { transition = Resolve; coinThreshold = 0.9 };
            { transition = DecreaseTensionToSubstitute; coinThreshold = 0.95 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | DominantSuspended4 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.3 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.5 };
            { transition = Resolve; coinThreshold = 0.9 };
            { transition = DecreaseTensionToSubstitute; coinThreshold = 0.95 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | DominantSubstitute ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.3 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.5 };
            { transition = Resolve; coinThreshold = 0.9 };
            { transition = DecreaseTensionToSubstitute; coinThreshold = 0.95 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | DominantSubstituteSuspended2 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.3 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.5 };
            { transition = Resolve; coinThreshold = 0.9 };
            { transition = DecreaseTensionToSubstitute; coinThreshold = 0.95 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]
    | DominantSubstituteSuspended4 ->
        [|
            { transition = Dublicate; coinThreshold = 0.1 };
            { transition = ResolveToFirstSubstitute; coinThreshold = 0.3 };
            { transition = ResolveToSecondSubstitute; coinThreshold = 0.5 };
            { transition = Resolve; coinThreshold = 0.9 };
            { transition = DecreaseTensionToSubstitute; coinThreshold = 0.95 };
            { transition = DecreaseTension; coinThreshold = 1.0 };
            { transition = AddColor2; coinThreshold = 1.5 };
            { transition = AddColor2; coinThreshold = 2.0 };
        |]

let applyCommand command chord =
    match command with
    | Dublicate -> dublicate chord
    | IncreaseTension -> increaseTension chord
    | IncreaseTensionToSubstitute -> increaseTensionToSubstitute chord
    | DecreaseTension -> decreaseTension chord
    | DecreaseTensionToSubstitute -> decreaseTensionToSubstitute chord
    | MaximizeTension -> maximizeTension chord
    | MaximizeTensionToSubstitute -> maximizeTensionToSubstitute chord
    | Resolve -> resolve chord
    | ResolveToFirstSubstitute -> resolveToFirstSubstitute chord
    | ResolveToSecondSubstitute -> resolveToSecondSubstitute chord
    | AddColor2 -> addColor2 chord
    | AddColor4 -> addColor4 chord

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
            let coin = rnd.NextDouble()*2.0
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
        | (value, Diminished) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 3
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 6
                duration = 0.25
            }|]
        | (value, Suspended2) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 2
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 7
                duration = 0.25
            }|]
        | (value, Suspended4) -> [|
            {
                midiNote = rootNote + value
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 5
                duration = 0.25
            };
            {
                midiNote = rootNote + value + 7
                duration = 0.25
            }|]
    match item with
    | Tonic -> [|
        {
            midiNote = chord.[0].midiNote
            duration = 1.0
        };
        {
            midiNote = chord.[1].midiNote
            duration = 0.5
        };
        {
            midiNote = chord.[2].midiNote
            duration = 0.5
        }|]
    | TonicSubstitute2 -> [|
        {
            midiNote = chord.[0].midiNote
            duration = 0.5
        };
        chord.[1];
        chord.[2]|]
    | SubDominantSubstituteSuspended2 -> [|
        chord.[0];
        {
            midiNote = chord.[1].midiNote
            duration = 0.5
        };
        chord.[2]|]
    | SubDominantSubstituteSuspended4 -> [|
        chord.[0];
        {
            midiNote = chord.[1].midiNote
            duration = 0.5
        };
        chord.[2]|]
    | Dominant -> [|
        {
            midiNote = chord.[0].midiNote
            duration = 0.5
        };
        chord.[1];
        chord.[2]|]
    | _ -> chord
