module AuctionSite.Tests.SingleSealedBidOptionTests

open NUnit.Framework
open FsUnit
open AuctionSite.Domain

[<TestFixture>]
type SingleSealedBidOptionTests() =
    [<Test>]
    member _.``Can parse Vickrey options from string``() =
        let sampleTypStr = "Vickrey"
        let parsed = SingleSealedBidOptions.TryParse sampleTypStr
        parsed |> should equal (Some Vickrey)

    [<Test>]
    member _.``Can serialize Vickrey options to string``() =
        let serialized = Vickrey.ToString()
        serialized |> should equal "Vickrey"

    [<Test>]
    member _.``Can parse Blind options from string``() =
        let sampleTypStr = "Blind"
        let parsed = SingleSealedBidOptions.TryParse sampleTypStr
        parsed |> should equal (Some Blind)

    [<Test>]
    member _.``Can serialize Blind options to string``() =
        let serialized = Blind.ToString()
        serialized |> should equal "Blind"
        
    [<Test>]
    member _.``Returns None for invalid option string``() =
        let invalidStr = "InvalidOption"
        let parsed = SingleSealedBidOptions.TryParse invalidStr
        parsed |> should equal None
