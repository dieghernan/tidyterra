
import { readdir, readFile, mkdir, writeFile } from 'node:fs/promises';
import { resolve, relative, join, sep } from 'node:path';
import { cleanPage, includePage } from './content.mjs';
import { listItems } from './items.mjs';
import { createCloudflareClient } from './client.mjs';
import {
  createManifest,
  readManifest,
  writeManifest
} from './manifest.mjs';
import { reconcileManifest } from './reconcile.mjs';
import { executeSync } from './execute.mjs';
import { setTimeout as sleep } from 'node:timers/promises';

// Adapted from chulapa/scripts/sync-ai-search.mjs for pkgdown output.
const args = process.argv.slice(2);
const root = resolve(
  args.find(arg => !arg.startsWith('--')) || '../../docs'
);
const dryRun = args.includes('--dry-run');
const prune = args.includes('--prune');
const allBatches = args.includes('--all-batches');
const reconcile = args.includes('--reconcile');
const development = args.includes('--development');
const missingOnly = args.includes('--missing-only');

if (missingOnly && (prune || allBatches || reconcile)) {
  throw new Error('--missing-only cannot be combined with --prune, --all-batches, or --reconcile.');
}

const manifestPath = resolve(
  process.env.AI_SEARCH_MANIFEST ?? 'manifest.json'
);

const account = process.env.CLOUDFLARE_ACCOUNT_ID;
const token = process.env.CLOUDFLARE_API_TOKEN;

const base = `https://api.cloudflare.com/client/v4/accounts/${account}/ai-search/namespaces/default/instances/tidyterra-ai-search`;
const cf = createCloudflareClient(base, token);

async function files(dir) {
  const result = [];

  for (const entry of await readdir(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);

    if (
      entry.isDirectory() &&
      ['reference', 'articles'].includes(entry.name) &&
      dir === root
    ) {
      result.push(...await files(path));
    } else if (
      entry.isFile() &&
      includePage(relative(root, path).split(sep).join('/'))
    ) {
      result.push(path);
    }
  }

  return result.sort();
}

// Validate and clean the entire tree before making remote changes.
const documents = [];

for (const file of await files(root)) {
  const key = relative(root, file).split(sep).join('/');
  const html = cleanPage(await readFile(file, 'utf8'), key, { development });

  if (html) documents.push({ key, html });
}

if (
  !documents.some(doc => doc.key === 'index.html') ||
  !documents.some(doc => doc.key.startsWith('reference/'))
) {
  throw new Error(
    'Expected a complete pkgdown tree with index.html and reference pages.'
  );
}

const current = createManifest(documents);
const previous = await readManifest(manifestPath);

if (dryRun) {
  await mkdir('preview', { recursive: true });

  await writeFile(
    'preview/documents.json',
    JSON.stringify(documents, null, 2)
  );

  const changed = documents.filter(
    doc => previous?.[doc.key] !== current[doc.key]
  );

  console.log(
    `Validated ${documents.length} documents from ${root}.`
  );
  console.log(
    `${changed.length} documents differ from the local manifest.`
  );
  console.log('No remote changes.');
} else if (reconcile) {
  if (!account || !token) {
    throw new Error('Set CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_API_TOKEN.');
  }
  console.log(`Comparing ${documents.length} documents with Cloudflare.`);
  const { manifest, missing, different, ambiguous, unavailable } =
    await reconcileManifest(cf, documents, current);
  await writeManifest(manifestPath, manifest);
  console.log(
    `Verified ${Object.keys(manifest).length} exact matches, ` +
    `${different} different, ${missing} missing, ${ambiguous} ambiguous, ` +
    `${unavailable} unavailable for download.`
  );
} else {
  if (!account || !token) {
    throw new Error(
      'Set CLOUDFLARE_ACCOUNT_ID and CLOUDFLARE_API_TOKEN.'
    );
  }

  const maxUploads = Number(
    process.env.AI_SEARCH_MAX_UPLOADS ?? 5
  );

  if (
    !Number.isSafeInteger(maxUploads) ||
    maxUploads < 1 ||
    maxUploads > 5
  ) {
    throw new Error(
      'AI_SEARCH_MAX_UPLOADS must be an integer from 1 to 5.'
    );
  }

  const maxBatches = allBatches ? 100 : 1;
  const confirmedThisRun = new Set();
  let previousState = previous;
  let completed = false;

  for (let batch = 1; batch <= maxBatches; batch++) {
    console.log(`Starting batch ${batch}.`);

    // Refresh the remote inventory before each batch.
    const existing = await listItems(cf);

    const result = await executeSync({
      cf,
      documents,
      current,
      previous: previousState,
      existing,
      confirmedThisRun,
      manifestPath,
      prune,
      missingOnly,
      maxUploads
    });

    console.log(
      `Batch ${batch}: uploaded ${result.uploaded}, ` +
      `skipped ${result.skipped}, ` +
      `deleted ${result.deleted}, ` +
      `pending ${result.pending}.`
    );

    if (result.pending === 0) {
      completed = true;
      break;
    }

    if (batch === maxBatches) {
      break;
    }

    // Reload the confirmed state before continuing.
    previousState = await readManifest(manifestPath);

    // Small pause to avoid an immediate burst of requests.
    await sleep(2000);
  }

  if (allBatches && !completed) {
    throw new Error(
      `Synchronization did not complete after ${maxBatches} batches.`
    );
  }

  console.log(
    completed
      ? 'Synchronization complete. Indexing continues asynchronously.'
      : 'Documents remain pending.'
  );
}
