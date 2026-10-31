---
id: file-operation-tests-no-longer-420ad3b
created: 2026-10-31
stage: done
route: specifier,coder,qa
---

# File-operation tests no longer race on a shared temp file

The test 'Serialization Tests.File Operations.Can write and read commands' is flaky (failed 2 of 5 runs). Expecto runs tests in parallel, and it and 'Can read and write events' both delete and append to ./.tmp/test-sample-commands.jsonl (AuctionSite.Tests/SerializationTests.fs:14, used at lines 122 and 144). verify.conf runs dotnet test, so sf finish fails at random.

## Records

```text
type: created
from: operator
branch: swarm/file-operation-tests-no-longer-420ad3b
commit: 85b3470
route: specifier,coder,qa
```

```text
type: review
from: specifier
commit: 0bc0cf5
files: -
summary: Spec: per-invocation temp files for File Operations tests, stress QA
```

```text
type: approval
from: operator
```

```text
type: handoff
from: specifier
to: coder
branch: swarm/file-operation-tests-no-longer-420ad3b
commit: 0bc0cf5
files: -
summary: specifier work complete
```

```text
type: handoff
from: coder
to: qa
branch: swarm/file-operation-tests-no-longer-420ad3b
commit: 1542e1c
files: AuctionSite.Tests/SerializationTests.fs
summary: File Operations tests use per-invocation scratch files; stress and 5x full suite green
```

```text
type: handoff
from: qa
to: done
branch: swarm/file-operation-tests-no-longer-420ad3b
commit: 655daa9
files: -
summary: Q1-Q5 pass: stress 9,792/0 failed, dotnet test 5x green, CRAP/DRY clean
```
