export async function listItems(cf) {
  const existing = [];
  for (let page = 1; ; page++) {
    const body = await cf(`/items?source=builtin&page=${page}&per_page=50`);
    const result = body.result ?? body;
    const batch = Array.isArray(result) ? result : result.items;
    if (!Array.isArray(batch)) throw new Error('Unrecognized item listing; refusing to sync.');
    existing.push(...batch);
    const info = body.result_info ?? result.result_info;
    if (!info || !Number.isInteger(info.total_count)) {
      throw new Error('Missing pagination information; refusing to sync.');
    }
    if (existing.length >= info.total_count) break;
    if (!batch.length) throw new Error('Incomplete item listing; refusing to sync.');
  }
  return existing;
}

export async function uploadDocument(cf, { key, html }, existing) {
  // POST rejects duplicate keys. DELETE removes the old managed file and index
  // before uploading its replacement; PUT /items only schedules indexing.
  const form = new FormData();
  form.append('file', new Blob([html], { type: 'text/html' }), key);
  if (existing) {
    if (existing.source_id !== 'builtin' || existing.key !== key || !existing.id) {
      throw new Error(`Refusing to replace an unmanaged or unidentified item: ${key}`);
    }
    await cf(`/items/${encodeURIComponent(existing.id)}`, { method: 'DELETE' });
  }
  await cf('/items', { method: 'POST', body: form });
}
