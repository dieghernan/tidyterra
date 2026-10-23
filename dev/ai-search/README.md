# Documentation assistant

The pkgdown site uses Cloudflare AI Search instance `tidyterra-ai-search` in
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
zero indexing errors). Retrieval returned
relevant content, but generation testing returned HTTP 429 because the account's
daily free allocation of 10,000 Workers AI neurons was exhausted. Answer quality
still needs verification after the quota resets. `smoke.mjs`
runs three public chat checks, stops on the first API error and saves responses
under `preview/` for manual review. Successful HTTP responses alone do not
establish factual correctness.

Before publishing changes, test factual questions, a question outside the docs,
source links, keyboard operation and narrow viewports. Updating source files
does not publish the pkgdown site; the normal deployment workflow does that.
