
import test from 'node:test';
import assert from 'node:assert/strict';
import { executeSync } from './execute.mjs';

const key = 'reference/example.html';
const hash = 'new-hash';

const document = {
  key,
  html: '<html>New content</html>'
};

const remote = {
  id: 'remote-1',
  key,
  source_id: 'builtin'
};

function setup({
  previous = {},
  existing = [remote],
  failUpload = false
} = {}) {
  const saved = [];
  const calls = [];

  const options = {
    cf: async () => {
      throw new Error('Unexpected Cloudflare request');
    },
    documents: [document],
    current: { [key]: hash },
    previous,
    existing,
    manifestPath: 'test-manifest.json',
    save: async (_, state) => {
      saved.push(structuredClone(state));
    },
    upload: async (_, doc, item) => {
      calls.push({
        key: doc.key,
        existing: item?.id
      });

      if (failUpload) {
        throw new Error('POST failed (429)');
      }
    },
    log: () => {}
  };

  return { options, saved, calls };
}

test('unchanged document causes no upload', async () => {
  const ctx = setup({
    previous: { [key]: hash }
  });

  const result = await executeSync(ctx.options);

  assert.equal(result.uploaded, 0);
  assert.equal(result.skipped, 1);
  assert.equal(ctx.calls.length, 0);
});

test('missing-only uploads absent documents and leaves replacements pending', async () => {
  const { options, calls, saved } = setup();
  const absent = { key: 'reference/absent.html', html: '<p>New</p>' };
  const result = await executeSync({
    ...options,
    documents: [document, absent],
    current: { [key]: hash, [absent.key]: 'absent-hash' },
    missingOnly: true
  });

  assert.deepEqual(calls, [{ key: absent.key, existing: undefined }]);
  assert.deepEqual(result, { uploaded: 1, skipped: 0, deleted: 0, pending: 1 });
  assert.deepEqual(saved.at(-1), { [absent.key]: 'absent-hash' });
});

test('changed document updates the manifest', async () => {
  const ctx = setup({
    previous: { [key]: 'old-hash' }
  });

  const result = await executeSync(ctx.options);

  assert.equal(result.uploaded, 1);
  assert.deepEqual(ctx.saved[0], {});
  assert.deepEqual(ctx.saved.at(-1), {
    [key]: hash
  });
});

test('failed upload invalidates the previous hash', async () => {
  const ctx = setup({
    previous: { [key]: 'old-hash' },
    failUpload: true
  });

  await assert.rejects(
    executeSync(ctx.options),
    /POST failed \(429\)/
  );

  assert.deepEqual(ctx.saved.at(-1), {});
  assert.equal(ctx.calls.length, 1);
});

test('next execution restores a missing document', async () => {
  const ctx = setup({
    previous: { [key]: hash },
    existing: []
  });

  const result = await executeSync(ctx.options);

  assert.equal(result.uploaded, 1);
  assert.equal(ctx.calls[0].existing, undefined);
  assert.deepEqual(ctx.saved.at(-1), {
    [key]: hash
  });
});

test('completed uploads survive a later failure', async () => {
  const secondKey = 'reference/second.html';
  const saved = [];
  const uploaded = [];

  const options = {
    cf: async () => {},
    documents: [
      document,
      { key: secondKey, html: '<html>Second</html>' }
    ],
    current: {
      [key]: hash,
      [secondKey]: 'second-hash'
    },
    previous: {},
    existing: [],
    manifestPath: 'test-manifest.json',
    save: async (_, state) => {
      saved.push(structuredClone(state));
    },
    upload: async (_, doc) => {
      uploaded.push(doc.key);

      if (doc.key === secondKey) {
        throw new Error('Simulated failure');
      }
    },
    log: () => {}
  };

  await assert.rejects(
    executeSync(options),
    /Simulated failure/
  );

  assert.deepEqual(uploaded, [key, secondKey]);
  assert.deepEqual(saved.at(-1), {
    [key]: hash
  });
});


test('DELETE succeeds, POST fails, and next run recovers', async () => {
  const saved = [];
  const operations = [];
  let remoteExists = true;
  let failPost = true;

  const cf = async (path, options = {}) => {
    const method = options.method ?? 'GET';
    operations.push(method);

    if (method === 'DELETE') {
      remoteExists = false;
      return { success: true };
    }

    if (method === 'POST') {
      if (failPost) {
        throw new Error('Cloudflare request failed: POST /items (429)');
      }

      remoteExists = true;
      return { success: true };
    }

    throw new Error(`Unexpected request: ${method} ${path}`);
  };

  const makeOptions = (previous) => ({
    cf,
    documents: [document],
    current: { [key]: hash },
    previous,
    existing: remoteExists ? [remote] : [],
    manifestPath: 'test-manifest.json',
    save: async (_, state) => {
      saved.push(structuredClone(state));
    },
    log: () => {}
  });

  await assert.rejects(
    executeSync(makeOptions({ [key]: 'old-hash' })),
    /POST \/items \(429\)/
  );

  assert.equal(remoteExists, false);
  assert.deepEqual(saved.at(-1), {});

  failPost = false;

  const result = await executeSync(
    makeOptions(saved.at(-1))
  );

  assert.equal(result.uploaded, 1);
  assert.equal(remoteExists, true);
  assert.deepEqual(saved.at(-1), { [key]: hash });
  assert.deepEqual(operations, ['DELETE', 'POST', 'POST']);
});


test('maxUploads limits uploads and preserves pending documents', async () => {
  const documents = Array.from({ length: 8 }, (_, i) => ({
    key: `reference/page-${i}.html`,
    html: `<html>Page ${i}</html>`
  }));

  const current = Object.fromEntries(
    documents.map((doc, i) => [doc.key, `hash-${i}`])
  );

  const uploaded = [];
  const saved = [];

  const result = await executeSync({
    cf: async () => {
      throw new Error('Unexpected Cloudflare request');
    },
    documents,
    current,
    previous: {},
    existing: [],
    manifestPath: 'test-manifest.json',
    maxUploads: 5,
    upload: async (_, doc) => {
      uploaded.push(doc.key);
    },
    save: async (_, state) => {
      saved.push(structuredClone(state));
    },
    log: () => {}
  });

  assert.equal(result.uploaded, 5);
  assert.equal(result.skipped, 0);
  assert.equal(uploaded.length, 5);
  assert.equal(Object.keys(saved.at(-1)).length, 5);

  for (const doc of documents.slice(5)) {
    assert.equal(
      Object.hasOwn(saved.at(-1), doc.key),
      false
    );
  }
});

test('lagging inventory does not replay uploads or trigger early pruning', async () => {
  const documents = Array.from({ length: 12 }, (_, i) => ({
    key: `reference/page-${i}.html`,
    html: `<html>Page ${i}</html>`
  }));
  const current = Object.fromEntries(
    documents.map(doc => [doc.key, doc.key])
  );
  const stale = { id: 'stale', key: 'reference/stale.html', source_id: 'builtin' };
  const operations = [];
  const confirmedThisRun = new Set();
  let previous = {};

  const options = {
    cf: async (path, { method, body }) => {
      if (method === 'POST') {
        assert.equal(path, '/items');
        operations.push(['POST', body.get('file').name]);
      } else {
        operations.push([method, path]);
      }
      return { success: true };
    },
    documents,
    current,
    existing: [stale],
    manifestPath: 'test-manifest.json',
    prune: true,
    maxUploads: 5,
    save: async (_, state) => {
      previous = structuredClone(state);
    },
    log: () => {}
  };

  for (const [uploaded, pending, deleted] of [[5, 7, 0], [5, 2, 0], [2, 0, 1]]) {
    const result = await executeSync({ ...options, previous, confirmedThisRun });
    assert.deepEqual([result.uploaded, result.pending, result.deleted], [
      uploaded, pending, deleted
    ]);
  }

  assert.deepEqual(operations, [
    ...documents.map(doc => ['POST', doc.key]),
    ['DELETE', '/items/stale']
  ]);
  assert.deepEqual(previous, current);

  const result = await executeSync({
    ...options,
    previous,
    existing: documents.slice(1).map(({ key }) => ({
      key, id: key, source_id: 'builtin'
    }))
  });
  assert.equal(result.uploaded, 1);
  assert.equal(result.pending, 0);
  assert.deepEqual(operations.at(-1), ['POST', documents[0].key]);
});
