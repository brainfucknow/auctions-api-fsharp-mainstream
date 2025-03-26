module AuctionSite.Tests.EnglishAuctionTests

open System
open NUnit.Framework
open FsUnit
open AuctionSite.Domain
open AuctionSite.Money
open AuctionSite.Tests.SampleData
open AuctionSite.Tests.AuctionTestHelpers

[<TestFixture>]
type EnglishAuctionTests() =
    let options = TimedAscending.defaultOptions Currency.SEK
    let timedAscAuction = sampleAuctionOfType (TimedAscending options)
    let emptyAscAuctionState = Auction.emptyState timedAscAuction  |> function | Choice2Of2 s -> s | _ -> failwith "Expected TimedAscending state"
    let stateHandler = TimedAscending.stateHandler
    
    // Get test helpers
    let testHelpers = timedAscendingTests timedAscAuction emptyAscAuctionState stateHandler options
    let commonTests = testHelpers.CommonTests

    [<Test>]
    member _.``Can add bid to empty state``() =
        commonTests.CanAddBidToEmptyState()

    [<Test>]
    member _.``Can add second bid``() =
        commonTests.CanAddSecondBid()

    [<Test>]
    member _.``Can end auction``() =
        testHelpers.CanEndEmptyAuction()

    [<Test>]
    member _.``Ended with two bids has correct state``() =
        testHelpers.EndedWithTwoBidsHasCorrectState()

    [<Test>]
    member _.``Cannot bid after auction has ended``() =
        commonTests.CannotPlaceBidAfterAuctionHasEnded()

    [<Test>]
    member _.``Can get winner and price from an auction``() =
        testHelpers.CanGetWinnerAndPrice()

    [<Test>]
    member _.``Cannot place bid lower than highest bid``() =
        testHelpers.CannotPlaceBidLowerThanHighestBid()

    [<Test>]
    member _.``Can parse TimedAscending options from string``() =
        let sampleTypStr = "English|VAC0|VAC0|0"
        let sampleTyp = TimedAscending.defaultOptions Currency.VAC
        
        let parsed = TimedAscending.tryParseOptions sampleTypStr
        parsed |> should equal (Some sampleTyp)

    [<Test>]
    member _.``Can serialize TimedAscending options to string``() =
        let sampleTyp = TimedAscending.defaultOptions Currency.VAC
        let sampleTypStr = "English|VAC0|VAC0|0"
        
        let serialized = TimedAscending.optionsToString sampleTyp
        serialized |> should equal sampleTypStr

    [<Test>]
    member _.``Can deserialize options with values``() =
        let sampleWithValuesTypStr = "English|VAC10|VAC20|30"
        let sampleWithValuesTyp = { 
            ReservePrice = createAmount Currency.VAC 10L
            MinRaise = createAmount Currency.VAC 20L
            TimeFrame = TimeSpan.FromSeconds(30.0)
        }
        
        let parsed = TimedAscending.tryParseOptions sampleWithValuesTypStr
        parsed |> should equal (Some sampleWithValuesTyp)

    [<Test>]
    member _.``Can serialize options with values``() =
        let sampleWithValuesTyp = { 
            ReservePrice = createAmount Currency.VAC 10L
            MinRaise = createAmount Currency.VAC 20L
            TimeFrame = TimeSpan.FromSeconds(30.0)
        }
        let sampleWithValuesTypStr = "English|VAC10|VAC20|30"
        
        let serialized = TimedAscending.optionsToString sampleWithValuesTyp
        serialized |> should equal sampleWithValuesTypStr
