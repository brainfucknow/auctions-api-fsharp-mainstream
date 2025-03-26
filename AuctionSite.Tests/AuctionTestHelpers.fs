module AuctionSite.Tests.AuctionTestHelpers

open NUnit.Framework
open FsUnit
open AuctionSite.Domain
open AuctionSite.Tests.SampleData
open AuctionSite.Tests.AuctionStateTests

/// Common test methods for all auction types
let commonAuctionTests<'TState> 
    (auction: Auction) 
    (emptyState: 'TState) 
    (stateHandler: IState<'TState>) =
    
    {|
        CanAddBidToEmptyState = fun () ->
            let _, result1 = stateHandler.AddBid bid1 emptyState
            match result1 with
            | Ok () -> ()
            | Error err -> Assert.Fail (string err)
            
        CanAddSecondBid = fun () ->
            let state1, _ = stateHandler.AddBid bid1 emptyState
            let _, result2 = stateHandler.AddBid bid2 state1
            match result2 with
            | Ok () -> ()
            | Error err -> Assert.Fail (string err)
            
        CannotPlaceBidAfterAuctionHasEnded = fun () ->
            let state1, _ = stateHandler.AddBid bid1 emptyState
            let stateEnded = stateHandler.Inc sampleEndsAt state1
            
            let _, result = stateHandler.AddBid bid2 stateEnded
            match result with
            | Ok result -> Assert.Fail (string result)
            | Error err -> err |> should equal (AuctionHasEnded sampleAuctionId)
            
        NoWinnerWhenNoBidsPlaced = fun () ->
            let stateEndedWithNoBids = stateHandler.Inc sampleEndsAt emptyState
            
            let maybeAmountAndWinner = stateHandler.TryGetAmountAndWinner stateEndedWithNoBids
            maybeAmountAndWinner |> should equal None
            
        RunIncrementStateTests = fun () ->
            // Get the increment spec test methods
            let tests = incrementSpec emptyState stateHandler
            
            tests.CanIncrementTwice()
            tests.WontEndJustAfterStart()
            tests.WontEndJustBeforeEnd()
            tests.WontEndJustBeforeStart()
            tests.WillHaveEndedJustAfterEnd()
    |}

/// Common test methods for SingleSealedBid auction types
let singleSealedBidTests<'TState> 
    (auction: Auction) 
    (emptyState: 'TState) 
    (stateHandler: IState<'TState>)
    (options: SingleSealedBidOptions) =
    
    // Get common test methods
    let commonTests = commonAuctionTests auction emptyState stateHandler
    
    {|
        // Include all common tests
        CommonTests = commonTests
        
        // Add specific tests for SingleSealedBid auctions
        CanEndAuction = fun () ->
            let state1, _ = stateHandler.AddBid bid1 emptyState
            let state2, _ = stateHandler.AddBid bid2 state1
            let stateEndedAfterTwoBids = stateHandler.Inc sampleEndsAt state2
            
            match stateEndedAfterTwoBids with
            | DisclosingBids(bids, expiry, opts) ->
                bids.Length |> should equal 2
                expiry |> should equal sampleEndsAt
                opts |> should equal options
            | _ -> Assert.Fail("Expected DisclosingBids state")
                
        GetWinnerWithSingleBid = fun () ->
            let state1, _ = stateHandler.AddBid bid1 emptyState
            let stateEndedAfterOneBid = stateHandler.Inc sampleEndsAt state1
            
            let maybeAmountAndWinner = stateHandler.TryGetAmountAndWinner stateEndedAfterOneBid
            maybeAmountAndWinner |> should equal (Some(bidAmount1, buyer1.UserId))
    |}
    
/// Common test methods for TimedAscending auction types
let timedAscendingTests 
    (auction: Auction) 
    (emptyState: TimedAscendingState) 
    (stateHandler: IState<TimedAscendingState>)
    (options: TimedAscendingOptions) =
    
    // Get common test methods
    let commonTests = commonAuctionTests auction emptyState stateHandler
    
    {|
        // Include all common tests
        CommonTests = commonTests
        
        // Add specific tests for TimedAscending auctions
        CanEndEmptyAuction = fun () ->
            let emptyEndedAuctionState = stateHandler.Inc sampleEndsAt emptyState
            emptyEndedAuctionState |> should equal (HasEnded([], sampleEndsAt, options))
            
        EndedWithTwoBidsHasCorrectState = fun () ->
            let state1, _ = stateHandler.AddBid bid1 emptyState
            let state2, _ = stateHandler.AddBid bid2 state1
            let stateEndedAfterTwoBids = stateHandler.Inc sampleEndsAt state2
            
            stateEndedAfterTwoBids |> should equal (HasEnded([bid2; bid1], sampleEndsAt, options))
            
        CanGetWinnerAndPrice = fun () ->
            let state1, _ = stateHandler.AddBid bid1 emptyState
            let state2, _ = stateHandler.AddBid bid2 state1
            let stateEndedAfterTwoBids = stateHandler.Inc sampleEndsAt state2
            
            let maybeAmountAndWinner = stateHandler.TryGetAmountAndWinner stateEndedAfterTwoBids
            maybeAmountAndWinner |> should equal (Some(bidAmount2, buyer2.UserId))
            
        CannotPlaceBidLowerThanHighestBid = fun () ->
            let state1, _ = stateHandler.AddBid bid1 emptyState
            let state2, _ = stateHandler.AddBid bid2 state1
            
            let _, maybeFail = stateHandler.AddBid bid_less_than_2 state2
            match maybeFail with
            | Ok () -> Assert.Fail "Did not expect ok"
            | Error err -> err |> should equal (MustPlaceBidOverHighestBid bidAmount2)
    |}
