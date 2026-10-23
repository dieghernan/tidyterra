# Documentation assistant

The pkgdown site's experimental assistant uses Cloudflare AI Search instance `tidyterra-ai-search` in
namespace `default`. Its public chat component is pinned to version 0.0.43 in
`pkgdown/_pkgdown.yml`. The search API token is never sent to the browser.

The ingestion approach is adapted from Chulapa's `scripts/sync-ai-search.mjs`.
Only the published site's root home page, reference and articles are indexed.
Development and archived copies, navigation and sidebars are excluded. Source
URLs and absolute links are included for citations. The assistant does not run R.

## Update the index

After a successful `pkgdown-gh-pages` workflow, `ai-search.yaml` reads the
published `gh-pages` branch and updates the index, including deployments triggered
by version tags. It can also be run manually.
Configure these repository secrets before enabling automated indexing:

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

## Cloudflare settings

`settings.json` records the instance settings applied for this prototype.
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
The baseline does not meet the original answer-quality criteria: three
standalone/contextual R examples fail to execute and another uses the wrong
scale family. The maintainer approved publication as an experimental assistant;
these findings remain open quality limitations.

`smoke.mjs` runs three public chat checks and saves responses under `preview/`.
For the full evaluation, run `node evaluate.mjs` from this directory; optionally
pass case IDs from `evaluation-cases.json` to select a subset. Follow-up case
`10-followup` requires `09-followup-base` in the same run. Each run consumes live
API quota and writes its evidence to a new timestamped directory under
`evaluations/`.

After manually reviewing recorded R blocks, `node validate-examples.mjs` executes
the selected example cases in isolated R processes, with explicit fixtures for
the conversation cases. Its process logs include actual R errors and a success
marker; failures produce a nonzero exit status. `node summarize-evaluation.mjs`
checks cited URLs and totals API-reported usage. Successful HTTP responses and
executable examples alone do not establish factual correctness.

Before publishing changes, test factual questions, a question outside the docs,
source links, keyboard operation and narrow viewports. Updating source files
does not publish the pkgdown site; the normal deployment workflow does that.
