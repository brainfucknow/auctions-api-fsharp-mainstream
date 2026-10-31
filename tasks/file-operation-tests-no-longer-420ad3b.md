---
id: file-operation-tests-no-longer-420ad3b
created: 2026-10-31
---

# File-operation tests no longer race on a shared temp file

The test 'Serialization Tests.File Operations.Can write and read commands' is flaky (failed 2 of 5 runs). Expecto runs tests in parallel, and it and 'Can read and write events' both delete and append to ./.tmp/test-sample-commands.jsonl (AuctionSite.Tests/SerializationTests.fs:14, used at lines 122 and 144). verify.conf runs dotnet test, so sf finish fails at random.
