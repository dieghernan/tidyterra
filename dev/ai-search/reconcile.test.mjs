import assert from 'node:assert/strict';
import { test } from 'node:test';
import { createCloudflareClient } from './client.mjs';
import { createManifest } from './manifest.mjs';
import { reconcileManifest } from './reconcile.mjs';

test('reconciliation records exact byte matches using only GET requests', async () => {
  const documents = ['equal', 'different', 'missing', 'duplicate'].map(key => ({
    key,
    html: '<p>España</p>'
  }));
  const items = ['equal', 'different', 'duplicate', 'duplicate'].map((key, id) => ({
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
      return new Response(path === '/items/0/download' ? documents[0].html : 'old');
    }
  });

  assert.deepEqual(await reconcileManifest(cf, documents, createManifest(documents)), {
    manifest: createManifest(documents.slice(0, 1)),
    missing: 1,
    different: 1,
    ambiguous: 1
  });
  assert.deepEqual(requests, ['/items', '/items/0/download', '/items/1/download']);
});
