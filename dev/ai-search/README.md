# Documentation assistant

The pkgdown site's experimental assistant uses Cloudflare AI Search instance `tidyterra-ai-search` in
namespace `default`. Its public chat component is pinned to version 0.0.43 in
`pkgdown/_pkgdown.yml`. The search API token is never sent to the browser.

The ingestion approach is adapted from Chulapa's `scripts/sync-ai-search.mjs`.
Only the published site's root home page, reference and articles are indexed.
The GitHub workflow reads these from the root of the `gh-pages` branch;
development and archived copies, navigation and sidebars are excluded. Source
URLs and absolute links are included for citations. The assistant does not run R.

## Update the index

After any successful `pkgdown-gh-pages` workflow, `ai-search.yaml` reads the
published `gh-pages` branch, updates the index and applies the system prompt from
`settings.json` after a successful sync. This includes deployments triggered by
version tags. Automatic updates require the repository variable
`AI_SEARCH_AUTO_SYNC` to be set to `true`; the workflow can also be run manually.
The `reconcile` mode does not update the remote prompt.

Configure this repository variable and secrets before enabling automated updates:

- `AI_SEARCH_AUTO_SYNC`: set to `true` to enable updates after pkgdown deploys.
- `CLOUDFLARE_ACCOUNT_ID`: the account containing the tidyterra instance.
- `CLOUDFLARE_API_TOKEN_SEARCH`: a token with AI Search Edit and Run permissions.

Local commands, from this directory, using Node.js 24:

```sh
npm ci
npm test
node sync.mjs site --dry-run
node --env-file=.env sync.mjs site --prune
node smoke.mjs
```

Here `site` is a checkout of the published `gh-pages` branch. Alternatively pass
a complete pkgdown build directory. Use the release root, not `docs/dev`, to keep
source links and version information consistent. A local `.env` may contain
`CLOUDFLARE_ACCOUNT_ID` and `CLOUDFLARE_API_TOKEN`; it is ignored by Git.
Dry runs write cleaned documents to the ignored `preview` directory.

All documents are validated and the complete existing item list is read before
uploading. Existing built-in keys are deleted before uploading their replacement,
because POST rejects duplicate keys. A failed replacement stops the sync and may
leave that page absent until the next successful run.
Reads and deletes retry HTTP 429 responses up to five times, honoring
`Retry-After` with a cumulative wait budget of five minutes per operation.
A DELETE returning 404 after an overload retry is treated as completed, as
Cloudflare documents that the earlier delete may already have succeeded.
Other failures still stop the sync. Uploads are not automatically replayed.
`--prune` removes stale documentation keys only after all uploads succeed and a
complete item listing is available. Indexing is asynchronous; upload completion
does not establish that every document is searchable yet.

## Migrate from the development index

The manifest stores hashes, not document contents. `sync-from-backup` restores a
manifest backup; it does not restore or roll back the remote index.

With `AI_SEARCH_AUTO_SYNC=true`, the next successful `pkgdown-gh-pages` run on
the default branch automatically starts the migration. The workflow reads the
root of `gh-pages`, synchronizes stable documentation, and prunes stale keys
only after all upload batches complete. No manual `reconcile` or
`sync-and-prune` dispatch is required for this transition. A failed deployment
does not start the sync; an incomplete or failed sync must not be treated as a
completed migration.

Monitor the first automatic run. After it succeeds, wait until Cloudflare
reports indexing complete, then verify the expected stable document count,
zero indexing errors or duplicate keys, absence of `/dev/` sources, and
published-site citation links. A successful Actions run confirms the sync
completed, not that asynchronous indexing is searchable or that those checks
passed.

If the run fails, review its logs and remote inventory before retrying. A
replacement uses DELETE followed by POST and may temporarily leave a page
absent; `reconcile` can detect exact content Cloudflare accepted before a
failure. `sync-from-backup` restores only manifest hashes, not remote content.
A rollback to development requires explicitly syncing that source and pruning
stale stable-only keys only after its uploads complete.

## Cloudflare settings

`settings.json` records the instance settings for this prototype. The GitHub
workflow applies its system prompt and syncs documentation; it does not reconcile
other instance settings. Apply changes to the public endpoint, authorized hosts,
rate limit and MCP settings in the Cloudflare dashboard or through a supported
Cloudflare configuration command, then verify the instance settings there.
Changing `settings.json` alone does not apply those values remotely.
The public endpoint allows the published site and local preview hosts,
has a rate limit of 20 requests per minute and disables MCP. CORS is a browser
restriction, not authentication. Only public documentation belongs in this index.
The assistant's system prompt requires grounded answers, source links and an
explicit admission when the documentation is insufficient. Conversation query
rewriting is disabled in the widget to keep the first prototype's usage simple.

The widget explains that questions are processed by Cloudflare and responses can
contain mistakes. Monitor AI Search and model usage in the Cloudflare dashboard;
generation costs are separate from retrieval allowances.

The widget inherits gitdevr's Bootstrap color variables, body and monospace fonts,
heading font and border radius. These stay synchronized with the site's light
and dark modes. When minimized, the chat body is inert while the visible header
controls remain available to keyboard users.

The initial upload indexed all 80 pages from the published site (276 vectors,
zero indexing errors). The first quality evaluation after the quota reset found
6 passing, 2 partial and 4 failing answers. See the
[evaluation report](evaluations/2026-10-23-review.md) and recorded API responses.
The [final evaluation](evaluations/2026-11-04-review.md) found 8 passing,
2 partial and 2 failing answers. RGB plotting, hypsometric tints and two-column
facets passed. Raster layer selection, the vector filtering example and
resolution claims around `maxcell = Inf` remain quality limitations.

`smoke.mjs` runs three public chat requests and saves responses under `preview/`.
Run it only after Cloudflare reports indexing complete. It checks HTTP responses
but does not wait for indexing or automatically validate source URLs, citations
or answer quality; inspect the saved responses.
For the full evaluation, run `node evaluate.mjs` from this directory; optionally
pass case IDs from `evaluation-cases.json` to select a subset. Follow-up case
`10-followup` requires `09-followup-base` in the same run. Each run consumes live
API quota and writes its evidence to a new timestamped directory under
`evaluations/`.

The suite contains 20 cases. Cases 01–12 retain the original questions for
comparison with the recorded evaluations. Cases 13–20 add raster layer
selection and renaming, value extraction, missing values, vector aggregation,
row binding across CRSs, categorical colors, raster coercion and CRS handling.
The [extended evaluation](evaluations/2026-11-05-review.md) covers cases 13–20
with 3 passes, 2 partial answers and 3 failures. Combined with the previous
12-case evaluation, the latest recorded results total 11 passes, 4 partial
answers and 5 failures out of 20 across two evaluation dates.

After manually reviewing recorded R blocks, `node validate-examples.mjs` executes
the selected example cases in isolated R processes, with explicit fixtures for
the conversation cases. Its process logs include actual R errors and a success
marker; failures produce a nonzero exit status. `node summarize-evaluation.mjs`
checks cited URLs and totals API-reported usage. Successful HTTP responses and
executable examples alone do not establish factual correctness.

Before publishing changes, test factual questions, a question outside the docs,
source links, keyboard operation and narrow viewports. Updating source files
does not publish the pkgdown site; the normal deployment workflow does that.
