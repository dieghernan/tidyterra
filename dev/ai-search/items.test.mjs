import assert from 'node:assert/strict';
import test from 'node:test';
import { listItems, uploadDocument } from './items.mjs';

test('two successive uploads replace content without duplicate-key conflicts', async () => {
  let item;
  let stored;
  const cf = async (path, options) => {
    if (options.method === 'DELETE') {
      assert.equal(path, '/items/existing-id');
      item = undefined;
      stored = undefined;
    } else {
      assert.equal(path, '/items');
      assert.equal(item, undefined, 'POST must not receive an existing key');
      const file = options.body.get('file');
      item = { id: 'existing-id', key: file.name, source_id: 'builtin' };
      stored = await file.text();
    }
  };
  await uploadDocument(cf, { key: 'index.html', html: 'first content' });
  assert.equal(stored, 'first content');
  await uploadDocument(cf, { key: 'index.html', html: 'updated content' }, item);
  assert.equal(stored, 'updated content');
});

test('failed deletion aborts replacement before POST', async () => {
  const requests = [];
  const cf = async (path, options) => {
    requests.push(options.method);
    throw new Error('Delete failed');
  };
  await assert.rejects(uploadDocument(cf, { key: 'index.html', html: 'new' }, {
    id: 'id', key: 'index.html', source_id: 'builtin'
  }), /Delete failed/);
  assert.deepEqual(requests, ['DELETE']);
});

test('unmanaged items cannot be deleted for replacement', async () => {
  await assert.rejects(uploadDocument(() => assert.fail('No request expected'), {
    key: 'index.html', html: 'new'
  }, { id: 'id', key: 'index.html', source_id: 'r2:other' }), /Refusing to replace/);
});

test('list all pages before syncing, even without total_pages', async () => {
  const requests = [];
  const result = await listItems(async path => {
    requests.push(path);
    return { result: [{ id: requests.length }], result_info: { total_count: 2 } };
  });
  assert.deepEqual(result, [{ id: 1 }, { id: 2 }]);
  assert.match(requests[1], /page=2&/);
});

test('incomplete listings fail before any document can be replaced', async () => {
  await assert.rejects(listItems(async () => ({ result: [] })), /Missing pagination/);
  await assert.rejects(listItems(async () => ({
    result: [], result_info: { total_count: 2 }
  })), /Incomplete item listing/);
});
