module ChordGeneratorTests

open Xunit
open Domain

type Tests() =
    static member testData : obj[] list = [
        // Tonic
        [| HarmonyItem.Tonic; 0.05; HarmonyItem.Tonic |]
        [| HarmonyItem.Tonic; 0.4; HarmonyItem.SubDominant |]
        [| HarmonyItem.Tonic; 0.91; HarmonyItem.Dominant |]

        // SubDominant
        [| HarmonyItem.SubDominant; 0.04; HarmonyItem.SubDominant |]
        [| HarmonyItem.SubDominant; 0.5; HarmonyItem.Dominant |]
        [| HarmonyItem.SubDominant; 0.8; HarmonyItem.Tonic |]

        // Dominant
        [| HarmonyItem.Dominant; 0.03; HarmonyItem.Dominant |]
        [| HarmonyItem.Dominant; 0.37; HarmonyItem.Tonic |]
        [| HarmonyItem.Dominant; 0.95; HarmonyItem.SubDominant |]
    ]

    [<Theory>]
    [<MemberData(nameof(Tests.testData))>]
    member this.``Test generateNextChord`` currentChord coin expected =
        let result = generateNextChord currentChord coin
        Assert.Equal(expected, result)
