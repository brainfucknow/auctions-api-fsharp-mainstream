module AuctionSite.Tests.VickreyAuctionTests

open NUnit.Framework
open FsUnit
open AuctionSite.Domain
open AuctionSite.Tests.SampleData
open AuctionSite.Tests.AuctionStateTests
open AuctionSite.Tests.AuctionTestHelpers

[<TestFixture>]
type VickreyAuctionTests() =
    // Create a Vickrey auction for testing
    let vickreyAuction = sampleAuctionOfType (SingleSealedBid Vickrey)
    let emptyVickreyAuctionState = Auction.emptyState vickreyAuction |> function | Choice1Of2 s -> s | _ -> failwith "Expected SingleSealedBid state"
    let stateHandler = SingleSealedBid.stateHandler
    
    // Get test helpers
    let testHelpers = singleSealedBidTests vickreyAuction emptyVickreyAuctionState stateHandler Vickrey
    let commonTests = testHelpers.CommonTests

    [<Test>]
    member _.``Can add bid to empty state``() =
        commonTests.CanAddBidToEmptyState()

    [<Test>]
    member _.``Can add second bid``() =
        commonTests.CanAddSecondBid()

    [<Test>]
    member _.``Can end auction``() =
        testHelpers.CanEndAuction()

    [<Test>]
    member _.``Cannot place bid after auction has ended``() =
        commonTests.CannotPlaceBidAfterAuctionHasEnded()

    [<Test>]
    member _.``Increment state tests``() =
        commonTests.RunIncrementStateTests()

    [<Test>]
    member _.``Get winner and price from an ended auction - winner pays second highest bid``() =
        // In Vickrey auction, the highest bidder wins but pays the second-highest bid amount
        let state1, _ = stateHandler.AddBid bid1 emptyVickreyAuctionState
        let state2, _ = stateHandler.AddBid bid2 state1  // bid2 is higher than bid1
        let stateEndedAfterTwoBids = stateHandler.Inc sampleEndsAt state2
        
        let maybeAmountAndWinner = stateHandler.TryGetAmountAndWinner stateEndedAfterTwoBids
        // Winner should be buyer2 (who placed bid2) but they pay the price of bid1
        maybeAmountAndWinner |> should equal (Some(bidAmount1, buyer2.UserId))

    [<Test>]
    member _.``Get winner and price from an ended auction with single bid``() =
        // When there's only one bid, the bidder pays their own bid amount
        testHelpers.GetWinnerWithSingleBid()

    [<Test>]
    member _.``No winner when no bids placed``() =
        commonTests.NoWinnerWhenNoBidsPlaced()
