# Project Rules

## Project Shape
- This project runs SwarmForge roles as harness agents. `swarmforge/roles.conf` is the role catalogue in canonical order; `swarmforge/routes.conf` names the routes a task can take through it; `swarmforge/tiers.conf` maps each role's tier to a model per harness.
- Project language: F# on .NET 9 (SDK-style `.fsproj`), with Giraffe for the Web API and Expecto (plus Expecto.FsCheck) for tests. Use `swarmforge/bin/sf-fs`; see `swarmforge/languages/fsharp.md`.
- Existing code: refactor freely. The opt-in value is `ask before refactoring`, which turns on the engineering article's Existing Code section; a project opts in by changing this line. A missing line means `refactor freely`.

## Local Configuration
- Keep SwarmForge configuration under `swarmforge/`, task worktrees under `.worktrees/`, and task state in the `swarm/*` branches and `tasks/` (`tasks/done/` once a task is done). The orphan branch `swarmforge/metrics` caches mutation manifests. Nothing else is swarm state.
- `.worktrees/` and `./tmp/` are ignored by git and never committed.

## Verification
- Public interfaces: the HTTP API in `AuctionSite.WebApi` (`GET /auctions`, `GET /auctions/{id}`, `POST /auctions`, `POST /auctions/{id}/bids`, authenticated by the base64 `x-jwt-payload` header) and the JSON Lines event file read and written by `AuctionSite.Persistence` (`EVENTS_FILE`, default `tmp/events.jsonl`). `README.md` describes both.
- Verification commands: `dotnet build`, then `swarmforge/bin/sf-fs test` (all Expecto tests through `dotnet test`); `swarmforge/bin/sf-fs coverage`, `crap` and `dry` for the quality gates. `dotnet run --project AuctionSite.Tests` runs the same suite through Expecto's own runner.
- Property tests: Expecto has no test categories, so `sf-fs property` (`Category=Property`) selects nothing and `sf-fs test` also runs properties. Name property test lists `Property` and run them with `dotnet test --filter FullyQualifiedName~Property.`.
- Mutation testing is unavailable for F# (`sf-fs mutate` exits 2); the hardener relies on CRAP, DRY and property tests.
- Apply the engineering article's target-based verification and evidence-reuse rules. Fix applicable failures before handoff.
- The architect, hardener, and QA own property-test verification for affected behavior; keep it separate from normal unit, coverage, and mutation runs.

## Handoffs
- Prefer terse, explicit handoffs that report state. Do not include verification transcripts or process narrative in the summary.

## Ownership
- Do not change another role's prompt or workflow ownership without explicit operator direction.
