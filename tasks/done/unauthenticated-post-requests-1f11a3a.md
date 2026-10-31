---
id: unauthenticated-post-requests-1f11a3a
created: 2026-10-31
stage: done
route: specifier,coder,qa
---

# Unauthenticated POST requests return 401 instead of 500

POST /auctions and POST /auctions/{id}/bids bind the JSON body before checking the x-jwt-payload header (AuctionSite.WebApi/App.fs:60 and :106). Observed: no auth with a malformed body gives 500, no auth with {} gives 500, and auth with a malformed body gives 500. Unauthenticated requests should get 401 whatever the body, and an authenticated request with a malformed body should not be a 500.

## Records

```text
type: created
from: operator
branch: swarm/unauthenticated-post-requests-1f11a3a
commit: 78d86db
route: specifier,coder,qa
```

```text
type: question
from: specifier
question: 1. Authenticated POST /auctions or POST /auctions/{id}/bids whose body cannot be read as the request (not JSON, empty body, {} missing required fields such as amount or id, or a field of the wrong type): which status? Options: (a) 400 Bad Request [recommended, matches the existing domain-error responses], (b) 422 Unprocessable Entity. 2. Authenticated POST /auctions/{id}/bids with such a body to an auction id that does not exist: (a) 400, body checked before the auction lookup [recommended, today's order], (b) 404, auction looked up first, (c) leave unspecified. 3. Should the error response body be constrained? Options: (a) status only, but no exception text or stack trace in the body [recommended], (b) a specific message or JSON shape (please give it), (c) status only. I will also treat a present but undecodable x-jwt-payload header as unauthenticated (401 whatever the body) and require that no event is recorded for any of these rejections; say if either is wrong.
```

```text
type: answer
from: operator
answer: 1a: 400 Bad Request for an authenticated request whose body cannot be read as the request. 2a: 400, body checked before the auction lookup. 3a: status only, but no exception text or stack trace in the body. Both assumptions are correct: a present but undecodable x-jwt-payload header is unauthenticated (401 whatever the body), and no event is recorded for any of these rejections.
```

```text
type: review
from: specifier
commit: 755ce1a
files: -
summary: Spec AC1-AC7 and QA Q1-Q5 in commit body: 401 before body, 400 for unreadable body, no event
```

```text
type: approval
from: operator
```

```text
type: handoff
from: specifier
to: coder
branch: swarm/unauthenticated-post-requests-1f11a3a
commit: 755ce1a
files: -
summary: specifier work complete
```

```text
type: handoff
from: coder
to: qa
branch: swarm/unauthenticated-post-requests-1f11a3a
commit: 0044ad0
files: AuctionSite.Tests/ApiTests.fs,AuctionSite.WebApi/App.fs
summary: POST endpoints authenticate first (401), then 400 on unreadable body; 188 tests pass
```

```text
type: handoff
from: qa
to: done
branch: swarm/unauthenticated-post-requests-1f11a3a
commit: d6eeb3a
files: -
summary: QA passed: AC1-AC7 and Q1-Q5 verified on live API; CRAP/DRY clean; no fixes
```
