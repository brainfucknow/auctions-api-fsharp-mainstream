namespace AuctionSite.Domain

open System
open AuctionSite.Money

/// Options for TimedAscending (English) auctions
type TimedAscendingOptions = {
    /// Minimum sale price. If the final bid does not reach this price, the item remains unsold.
    ReservePrice: Amount
    
    /// Minimum amount by which the next bid must exceed the current highest bid.
    MinRaise: Amount
    
    /// Time frame after which the auction ends if no new bids are placed.
    TimeFrame: TimeSpan
}

/// State for TimedAscending auctions
type TimedAscendingState =
    /// The auction hasn't started yet
    | AwaitingStart of DateTime * DateTime * TimedAscendingOptions
    /// The auction is in progress
    | OnGoing of Bid list * DateTime * TimedAscendingOptions
    /// The auction has ended
    | HasEnded of Bid list * DateTime * TimedAscendingOptions

/// Functions for working with TimedAscending auctions
module TimedAscending =
    /// Create default options for a TimedAscending auction
    let defaultOptions (currency: Currency) =
        {
            ReservePrice = { Currency = currency; Value = 0L }
            MinRaise = { Currency = currency; Value = 0L }
            TimeFrame = TimeSpan.Zero
        }
        
    /// Create an empty state for a TimedAscending auction
    let emptyState (startsAt: DateTime) (expiry: DateTime) (options: TimedAscendingOptions) : TimedAscendingState =
        AwaitingStart(startsAt, expiry, options)
        
    /// Parse TimedAscendingOptions from string
    let tryParseOptions (s: string) =
        let parseTimeFrame (secondsStr: string) =
            match Int32.TryParse secondsStr with
            | true, seconds -> Some(TimeSpan.FromSeconds(float seconds))
            | _ -> None
            
        let parts = s.Split('|')
        if parts.Length = 4 && parts[0] = "English" then
            match tryParseAmount parts[1], tryParseAmount parts[2], parseTimeFrame parts[3] with
            | Some reservePrice, Some minRaise, Some timeFrame ->
                if reservePrice.Currency = minRaise.Currency then
                    Some {
                        ReservePrice = reservePrice
                        MinRaise = minRaise
                        TimeFrame = timeFrame
                    }
                else None
            | _ -> None
        else None
        
    /// Convert TimedAscendingOptions to string
    let optionsToString (options: TimedAscendingOptions) =
        let seconds = int options.TimeFrame.TotalSeconds
        $"English|%s{string options.ReservePrice}|%s{string options.MinRaise}|%d{seconds}"
    
    /// Implementation of the IState interface for TimedAscendingState
    let rec stateHandler =
        { new IState<TimedAscendingState> with
            member _.Inc (now: DateTime) (state: TimedAscendingState) =
                let transitionToEnded bids expiry opt =
                    HasEnded(bids, expiry, opt)
                    
                match state with
                | AwaitingStart(start, startingExpiry, opt) ->
                    if now > start then
                        if now < startingExpiry then
                            // Transition from AwaitingStart to OnGoing
                            OnGoing([], startingExpiry, opt)
                        else
                            // Transition directly from AwaitingStart to HasEnded
                            transitionToEnded [] startingExpiry opt
                    else
                        // Stay in AwaitingStart
                        state
                | OnGoing(bids, nextExpiry, opt) ->
                    if now < nextExpiry then
                        // Stay in OnGoing
                        state
                    else
                        // Transition from OnGoing to HasEnded
                        transitionToEnded bids nextExpiry opt
                | HasEnded _ ->
                    // Stay in HasEnded
                    state
                    
            member _.AddBid (bid: Bid) (state: TimedAscendingState) =
                let now = bid.At
                let auctionId = bid.ForAuction
                let bidAmount = bid.BidAmount
                
                // Advance state to current time
                let nextState = stateHandler.Inc now state
                
                match nextState with
                | AwaitingStart _ ->
                    nextState, Error(AuctionHasNotStarted auctionId)
                | OnGoing(bids, nextExpiry, opt) ->
                    let extendExpiry () = max nextExpiry (now.Add(opt.TimeFrame))
                    
                    match bids with
                    | [] ->
                        // First bid - extend expiry if needed
                        let nextExpiry' = extendExpiry()
                        OnGoing(bid :: bids, nextExpiry', opt), Ok()
                    | highestBid :: _ ->
                        // Check if bid is high enough
                        let highestBidAmount = highestBid.BidAmount
                        let nextExpiry' = extendExpiry()
                        let minRaiseAmount = opt.MinRaise
                        let minimumBid = add highestBidAmount minRaiseAmount
                        
                        if isGreaterThan bidAmount minimumBid then
                            // Bid is high enough
                            OnGoing(bid :: bids, nextExpiry', opt), Ok()
                        else
                            // Bid is too low
                            nextState, Error(MustPlaceBidOverHighestBid highestBidAmount)
                | HasEnded _ ->
                    nextState, Error(AuctionHasEnded auctionId)
                    
            member _.GetBids (state: TimedAscendingState) =
                match state with
                | OnGoing(bids, _, _) -> bids
                | HasEnded(bids, _, _) -> bids
                | AwaitingStart _ -> []
                    
            member _.TryGetAmountAndWinner (state: TimedAscendingState) =
                let getWinnerFromBids bids opt =
                    match bids with
                    | bid :: _ when isGreaterThan bid.BidAmount opt.ReservePrice ->
                        Some(bid.BidAmount, bid.Bidder.UserId)
                    | _ -> None
                
                match state with
                | HasEnded(bids, _, opt) -> getWinnerFromBids bids opt
                | _ -> None
                        
            member _.HasEnded (state: TimedAscendingState) =
                match state with
                | HasEnded _ -> true
                | _ -> false
        }
