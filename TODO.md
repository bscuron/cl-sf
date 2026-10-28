* TODO

1. Setup authentication using same auth method that the sf cli does without using the sf cli.
2. Setup endpoints
3. Come up with a way that the user of this lib doesnt need to worry about pagination when dealing with endpoints (auto laziness?, other ideas?)
4. First class support for bulk data operations (automatically determine if something needs to use the bulk api?)
5. Salesforce Standard Objects and Custom Objects translated to CLOS objects

* Design Decisions

** JSON — com.inuoe.jzon
Benchmarked against yason, cl-json, jonathan, shasht, boost-json on SBCL.
jzon has the shortest read time and is the only library that is fully
RFC-8259 compliant (declines all invalid inputs per the JSONTestSuite).

** HTTP — dexador
Standard, well-maintained async-capable CL HTTP/1.1 client.

** Parallelism — lparallel
Used in three places:
  - Pagination (TODO #3): %FETCH-NEXT-PAGE! fires a LPARALLEL:FUTURE for
    the next page immediately after filling the current buffer (one page of
    read-ahead). COLLECT-ALL uses LPARALLEL:PMAPCAR when the page count is
    knowable to fetch all pages in parallel.
  - Bulk (TODO #4): %POLL-JOB runs inside a LPARALLEL:FUTURE so the caller
    is never blocked. Multiple concurrent bulk operations can be submitted
    and waited on in parallel.
  - Objects (TODO #5): LOAD-SOBJECT-CLASSES uses LPARALLEL:PMAPCAR to
    fire all DESCRIBE-SOBJECT calls concurrently, reducing startup time
    when loading a full org's object schema.

** OpenAPI 3.0 (Salesforce Beta)
NOT used as the foundation of the library — the spec is Beta, subject to
change, and covers only sObjects (no Bulk, Composite, Tooling, or auth).
Used OPTIONALLY at runtime only, as an alternative introspection path for
LOAD-SOBJECT-CLASSES/OPENAPI (TODO #5). The primary path remains
DESCRIBE-SOBJECT via the stable REST API. Falls back automatically on any
error from the OpenAPI endpoint.

** Pagination strategy (TODO #3)
SELECT is eager: it follows nextRecordsUrl until the result set is
exhausted and returns one simple-vector of SOBJECTs. Pages chain by cursor
URL, so they are fetched serially.

** Bulk auto-routing (TODO #4)
DML operations are automatically routed to Bulk 2.0 when the record count
exceeds *BULK-THRESHOLD* (default: 200). Threshold is a dynamic variable
and can be tuned per call site with LET bindings.
