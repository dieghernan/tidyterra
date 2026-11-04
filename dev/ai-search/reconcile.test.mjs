import assert from 'node:assert/strict';
import { test } from 'node:test';
import { createCloudflareClient } from './client.mjs';
import { createManifest } from './manifest.mjs';
import { reconcileManifest } from './reconcile.mjs';

test('reconciliation records exact byte matches using only GET requests', async () => {
  const documents = ['unavailable', 'equal', 'different', 'missing', 'duplicate'].map(key => ({
    key,
    html: '<p>España</p>'
  }));
  const items = ['unavailable', 'equal', 'different', 'duplicate', 'duplicate'].map((key, id) => ({
    key,
    id: String(id),
    source_id: 'builtin'
  }));
  const requests = [];
  const cf = createCloudflareClient('https://example.test', 'fake', {
    fetchImpl: async (url, { method }) => {
      assert.equal(method ?? 'GET', 'GET');
      const path = new URL(url).pathname;
      requests.push(path);
      if (path === '/items') {
        return Response.json({
          result: items,
          result_info: { total_count: items.length }
        });
      }
      if (path === '/items/0/download') return new Response(null, { status: 404 });
      return new Response(path === '/items/1/download' ? documents[1].html : 'old');
    }
  });

  assert.deepEqual(await reconcileManifest(cf, documents, createManifest(documents)), {
    manifest: createManifest(documents.slice(1, 2)),
    missing: 1,
    different: 1,
    ambiguous: 1,
    unavailable: 1
  });
  assert.deepEqual(requests, [
    '/items', '/items/0/download', '/items/1/download', '/items/2/download'
  ]);
});
