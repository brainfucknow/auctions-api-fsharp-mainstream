module AuctionSite.Tests.BlindAuctionTests

open NUnit.Framework
open FsUnit
open AuctionSite.Domain
open AuctionSite.Tests.SampleData
open AuctionSite.Tests.AuctionStateTests
open AuctionSite.Tests.AuctionTestHelpers

[<TestFixture>]
type BlindAuctionTests() =
    // Create a Blind auction for testing
    let blindAuction = sampleAuctionOfType (SingleSealedBid Blind)
    let emptyBlindAuctionState = Auction.emptyState blindAuction |> function | Choice1Of2 s -> s | _ -> failwith "Expected SingleSealedBid state"
    let stateHandler = SingleSealedBid.stateHandler
    
    // Get test helpers
    let testHelpers = singleSealedBidTests blindAuction emptyBlindAuctionState stateHandler Blind
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
    member _.``Get winner and price from an ended auction - winner pays their own bid``() =
        // In Blind auction, the highest bidder wins and pays their own bid amount
        let state1, _ = stateHandler.AddBid bid1 emptyBlindAuctionState
        let state2, _ = stateHandler.AddBid bid2 state1  // bid2 is higher than bid1
        let stateEndedAfterTwoBids = stateHandler.Inc sampleEndsAt state2
        
        let maybeAmountAndWinner = stateHandler.TryGetAmountAndWinner stateEndedAfterTwoBids
        // Winner should be buyer2 (who placed bid2) and they pay their own bid amount
        maybeAmountAndWinner |> should equal (Some(bidAmount2, buyer2.UserId))

    [<Test>]
    member _.``Get winner and price from an ended auction with single bid``() =
        testHelpers.GetWinnerWithSingleBid()

    [<Test>]
    member _.``No winner when no bids placed``() =
        commonTests.NoWinnerWhenNoBidsPlaced()

    [<Test>]
    member _.``Cannot place bid after auction has ended``() =
        commonTests.CannotPlaceBidAfterAuctionHasEnded()

    [<Test>]
    member _.``Increment state tests``() =
        commonTests.RunIncrementStateTests()

    [<Test>]
    member _.``Bids are sorted by amount in descending order when ended``() =
        // Create bids with different amounts
        let lowBid = { bid1 with BidAmount = sek 5L }
        let midBid = { bid2 with BidAmount = sek 10L }
        let highBid = { 
            ForAuction = sampleAuctionId
            Bidder = buyer3
            At = sampleStartsAt.AddSeconds(3.0)
            BidAmount = sek 15L 
        }
        
        // Add bids in random order
        let state1, _ = stateHandler.AddBid midBid emptyBlindAuctionState
        let state2, _ = stateHandler.AddBid lowBid state1
        let state3, _ = stateHandler.AddBid highBid state2
        
        // End the auction
        let endedState = stateHandler.Inc sampleEndsAt state3
        
        // Check that bids are sorted by amount (highest first)
        match endedState with
        | DisclosingBids(bids, _, _) ->
            bids.Length |> should equal 3
            bids[0].BidAmount |> should equal (sek 15L)
            bids[1].BidAmount |> should equal (sek 10L)
            bids[2].BidAmount |> should equal (sek 5L)
        | _ -> Assert.Fail("Expected DisclosingBids state")
