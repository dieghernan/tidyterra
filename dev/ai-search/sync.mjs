import { readdir, readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve, relative, join, sep } from 'node:path';
import { cleanPage, includePage } from './content.mjs';

// Adapted from chulapa/scripts/sync-ai-search.mjs for pkgdown output.
const args = process.argv.slice(2);
const root = resolve(args.find(arg => !arg.startsWith('--')) || '../../docs');
const dryRun = args.includes('--dry-run');
const prune = args.includes('--prune');
const account = process.env.CLOUDFLARE_ACCOUNT_ID;
const token = process.env.CLOUDFLARE_API_TOKEN;
const base = `https://api.cloudflare.com/client/v4/accounts/${account}/ai-search/namespaces/default/instances/tidyterra-ai-search`;

async function files(dir) {
  const result = [];
  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory() && ['reference', 'articles'].includes(entry.name) && dir === root) {
      result.push(...await files(path));
    } else if (entry.isFile() && includePage(relative(root, path).split(sep).join('/'))) {
      result.push(path);
    }
  }
  return result.sort();
}

async function cf(path, options = {}) {
  const response = await fetch(base + path, {
    ...options,
    headers: { Authorization: `Bearer ${token}`, ...options.headers },
    signal: AbortSignal.timeout(120000)
  });
  const body = await response.json();
  if (!response.ok || body.success === false) {
    // Avoid logging credentials or server payloads in Actions.
    throw new Error(`Cloudflare request failed: ${options.method || 'GET'} ${path} (${response.status})`);
  }
  return body;
}

// Validate and clean the entire tree before making any remote changes.
const documents = [];
for (const file of await files(root)) {
  const key = relative(root, file).split(sep).join('/');
  const html = cleanPage(await readFile(file, 'utf8'), key);
  if (html) documents.push({ key, html });
}
if (!documents.some(doc => doc.key === 'index.html') ||
    !documents.some(doc => doc.key.startsWith('reference/'))) {
  throw new Error('Expected a complete pkgdown tree with index.html and reference pages.');
}

if (dryRun) {
  await mkdir('preview', { recursive: true });
  await writeFile('preview/documents.json', JSON.stringify(documents, null, 2));
  console.log(`Validated ${documents.length} documents from ${root}; no remote changes.`);
} else {
  if (!account || !token) throw new Error('Set CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_API_TOKEN.');
  for (const { key, html } of documents) {
    const form = new FormData();
    form.append('file', new Blob([html], { type: 'text/html' }), key);
    await cf('/items', { method: 'POST', body: form });
    console.log(`Uploaded ${key}`);
  }
  if (prune) {
    const wanted = new Set(documents.map(doc => doc.key));
    const existing = [];
    for (let page = 1; ; page++) {
      const body = await cf(`/items?source=builtin&page=${page}&per_page=50`);
      const result = body.result ?? body;
      const batch = Array.isArray(result) ? result : result.items;
      if (!Array.isArray(batch)) throw new Error('Unrecognized item listing; refusing to prune.');
      existing.push(...batch);
      const info = body.result_info ?? result.result_info;
      if (!info) throw new Error('Missing pagination information; refusing to prune.');
      if (page >= info.total_pages || existing.length >= info.total_count) break;
      if (!batch.length) throw new Error('Incomplete item listing; refusing to prune.');
    }
    for (const item of existing) {
      if (item.source_id === 'builtin' && item.id && includePage(item.key) && !wanted.has(item.key)) {
        await cf(`/items/${encodeURIComponent(item.id)}`, { method: 'DELETE' });
        console.log(`Deleted stale document ${item.key}`);
      }
    }
  }
  console.log(`Uploaded ${documents.length} documents. Indexing continues asynchronously.`);
}
