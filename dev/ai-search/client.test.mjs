import assert from 'node:assert/strict';
import test from 'node:test';
import { createCloudflareClient } from './client.mjs';
import { uploadDocument } from './items.mjs';

function client(responses, overrides = {}) {
  const requests = [];
  const waits = [];
  const cf = createCloudflareClient('https://example.invalid', 'secret', {
    fetchImpl: async (url, options) => {
      requests.push({ url, method: options.method || 'GET' });
      assert.ok(responses.length, 'Unexpected extra request');
      return responses.shift();
    },
    sleep: async ms => waits.push(ms), log: () => {}, ...overrides
  });
  return { cf, requests, waits };
}

const ok = () => Response.json({ success: true, result: [] });
const throttled = retryAfter => new Response('Temporarily overloaded', {
  status: 429, headers: retryAfter ? { 'Retry-After': retryAfter } : {}
});

test('replacement recovers when a throttled DELETE actually removed the item', async () => {
  const { cf, requests, waits } = client([throttled('2'), new Response('', { status: 404 }), ok()]);
  await uploadDocument(cf, { key: 'index.html', html: '<p>updated</p>' }, {
    id: 'old', key: 'index.html', source_id: 'builtin'
  });
  assert.deepEqual(requests.map(request => request.method), ['DELETE', 'DELETE', 'POST']);
  assert.deepEqual(waits, [2000]);
});

test('GET honors Retry-After HTTP dates', async () => {
  const { cf, waits } = client([throttled('Fri, 23 Oct 2026 16:00:03 GMT'), ok()], {
    now: () => Date.parse('Fri, 23 Oct 2026 16:00:00 GMT')
  });
  assert.deepEqual(await cf('/items'), { success: true, result: [] });
  assert.deepEqual(waits, [3000]);
});

test('missing or invalid Retry-After uses increasing delays', async () => {
  const { cf, waits } = client([throttled(), throttled('invalid'), ok()]);
  await cf('/items');
  assert.deepEqual(waits, [5000, 10000]);
});

test('persistent overload stops after five retries', async () => {
  const { cf, requests, waits } = client(Array.from({ length: 6 }, () => throttled('1')));
  await assert.rejects(cf('/items/old', { method: 'DELETE' }), /DELETE \/items\/old \(429\)/);
  assert.equal(requests.length, 6);
  assert.equal(waits.length, 5);
});

test('excessive Retry-After fails instead of retrying too soon', async () => {
  const { cf, requests, waits } = client([throttled('301')]);
  await assert.rejects(cf('/items'), /\(429\)/);
  assert.equal(requests.length, 1);
  assert.deepEqual(waits, []);
});

test('initial DELETE 404 and authorization failures are not hidden', async () => {
  for (const status of [404, 403]) {
    const { cf, requests, waits } = client([new Response('secret body', { status })]);
    await assert.rejects(cf('/items/old', { method: 'DELETE' }), error =>
      error.message.endsWith(`(${status})`) && !error.message.includes('secret'));
    assert.equal(requests.length, 1);
    assert.deepEqual(waits, []);
  }
});

test('POST overload is reported without replaying uploads', async () => {
  const { cf, requests, waits } = client([throttled('1')]);
  await assert.rejects(cf('/items', { method: 'POST' }), /POST \/items \(429\)/);
  assert.equal(requests.length, 1);
  assert.deepEqual(waits, []);
});
