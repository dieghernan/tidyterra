import { listItems } from './items.mjs';

export async function reconcileManifest(cf, documents, current) {
  const items = await listItems(cf);
  const remote = new Map();
  const duplicates = new Set();
  for (const item of items) {
    if (item.source_id !== 'builtin' || !item.id) continue;
    if (remote.has(item.key)) duplicates.add(item.key);
    remote.set(item.key, item.id);
  }

  const manifest = {};
  let missing = 0;
  let different = 0;
  let ambiguous = 0;
  let unavailable = 0;
  for (const { key, html } of documents) {
    if (duplicates.has(key)) {
      ambiguous++;
      continue;
    }
    const id = remote.get(key);
    if (!id) {
      missing++;
      continue;
    }
    let response;
    try {
      response = await cf(
        `/items/${encodeURIComponent(id)}/download`,
        { method: 'GET' },
        { raw: true }
      );
    } catch (error) {
      if (error.status !== 404) throw error;
      unavailable++;
      continue;
    }
    if (Buffer.from(await response.arrayBuffer()).equals(Buffer.from(html, 'utf8'))) {
      manifest[key] = current[key];
    } else {
      different++;
    }
  }
  return { manifest, missing, different, ambiguous, unavailable };
}
