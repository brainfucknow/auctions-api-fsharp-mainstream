namespace AuctionSite.Domain

open System
open AuctionSite.Money

/// Options for SingleSealedBid auctions
type SingleSealedBidOptions =
    /// Sealed first-price auction. The highest bidder pays the price they submitted.
    | Blind
    /// Sealed-bid second-price auction (Vickrey). The highest bidder pays the second-highest bid.
    | Vickrey
    
    override this.ToString() =
        match this with
        | Blind -> "Blind"
        | Vickrey -> "Vickrey"
        
    static member TryParse(s: string) =
        match s with
        | "Blind" -> Some Blind
        | "Vickrey" -> Some Vickrey
        | _ -> None

/// State for SingleSealedBid auctions
type SingleSealedBidState =
    /// State while the auction is accepting bids
    | AcceptingBids of Map<UserId, Bid> * DateTime * SingleSealedBidOptions
    /// State after the auction has ended and bids are disclosed
    | DisclosingBids of Bid list * DateTime * SingleSealedBidOptions

/// Functions for working with SingleSealedBid auctions
module SingleSealedBid =
    /// Create an empty state for a SingleSealedBid auction
    let emptyState (expiry: DateTime) (options: SingleSealedBidOptions) : SingleSealedBidState =
        AcceptingBids(Map.empty, expiry, options)
        
    /// Implementation of the IState interface for SingleSealedBidState
    let rec stateHandler =
        { new IState<SingleSealedBidState> with
            member _.Inc (now: DateTime) (state: SingleSealedBidState) =
                let transitionToEnded bids expiry opt =
                    // Sort bids by amount in descending order
                    let sortedBids = 
                        bids 
                        |> Map.toList 
                        |> List.map snd 
                        |> Bid.sortByAmountDescending
                    DisclosingBids(sortedBids, expiry, opt)
                    
                match state with
                | AcceptingBids(bids, expiry, opt) ->
                    if now >= expiry then
                        transitionToEnded bids expiry opt
                    else
                        state
                | DisclosingBids _ -> 
                    state
                    
            member _.AddBid (bid: Bid) (state: SingleSealedBidState) =
                let now = bid.At
                let auctionId = bid.ForAuction
                let user = bid.Bidder.UserId
                
                // Advance state to current time
                let nextState = stateHandler.Inc now state
                
                let validateAndAddBid bids expiry opt =
                    if bids.ContainsKey user then
                        nextState, Error AlreadyPlacedBid
                    else
                        let updatedBids = bids.Add(user, bid)
                        AcceptingBids(updatedBids, expiry, opt), Ok()
                
                match nextState with
                | AcceptingBids(bids, expiry, opt) ->
                    validateAndAddBid bids expiry opt
                | DisclosingBids _ ->
                    nextState, Error(AuctionHasEnded auctionId)
                    
            member _.GetBids (state: SingleSealedBidState) =
                match state with
                | AcceptingBids(bids, _, _) -> 
                    bids |> Map.toList |> List.map snd
                | DisclosingBids(bids, _, _) -> 
                    bids
                    
            member _.TryGetAmountAndWinner (state: SingleSealedBidState) =
                let getWinnerForBids bids options =
                    match bids, options with
                    | [], _ -> 
                        None
                    | highestBid :: secondHighest :: _, Vickrey ->
                        Some(secondHighest.BidAmount, highestBid.Bidder.UserId)
                    | [highestBid], _ ->
                        Some(highestBid.BidAmount, highestBid.Bidder.UserId)
                    | highestBid :: _, Blind ->
                        Some(highestBid.BidAmount, highestBid.Bidder.UserId)
                
                match state with
                | AcceptingBids _ -> 
                    None
                | DisclosingBids(bids, _, options) ->
                    getWinnerForBids bids options
                        
            member _.HasEnded (state: SingleSealedBidState) =
                match state with
                | AcceptingBids _ -> false
                | DisclosingBids _ -> true
        }
