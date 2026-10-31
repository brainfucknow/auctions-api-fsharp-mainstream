---
id: unauthenticated-post-requests-1f11a3a
created: 2026-10-31
---

# Unauthenticated POST requests return 401 instead of 500

POST /auctions and POST /auctions/{id}/bids bind the JSON body before checking the x-jwt-payload header (AuctionSite.WebApi/App.fs:60 and :106). Observed: no auth with a malformed body gives 500, no auth with {} gives 500, and auth with a malformed body gives 500. Unauthenticated requests should get 401 whatever the body, and an authenticated request with a malformed body should not be a 500.
