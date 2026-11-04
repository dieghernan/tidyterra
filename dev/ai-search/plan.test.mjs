
import test from 'node:test';
import assert from 'node:assert/strict';
import { planSync } from './plan.mjs';

const document = {
  key: 'reference/example.html',
  hash: 'abc123',
  html: '<html>Example</html>'
};

const remote = [{
  id: 'remote-1',
  key: document.key,
  source_id: 'builtin'
}];

test('unchanged remote document is skipped', () => {
  const result = planSync(
    [document],
    { [document.key]: document.hash },
    remote
  );

  assert.equal(result.upload.length, 0);
  assert.deepEqual(result.skip, [document.key]);
});

test('changed document is uploaded', () => {
  const result = planSync(
    [document],
    { [document.key]: 'old-hash' },
    remote
  );

  assert.equal(result.upload.length, 1);
  assert.equal(result.upload[0].existing.id, 'remote-1');
});

test('missing remote document is uploaded', () => {
  const result = planSync(
    [document],
    { [document.key]: document.hash },
    []
  );

  assert.equal(result.upload.length, 1);
  assert.equal(result.upload[0].existing, undefined);
});

test('missing manifest requires upload', () => {
  const result = planSync([document], null, remote);

  assert.equal(result.upload.length, 1);
});

test('unmanaged item is not treated as synchronized', () => {
  const result = planSync(
    [document],
    { [document.key]: document.hash },
    [{ ...remote[0], source_id: 'external' }]
  );

  assert.equal(result.upload.length, 1);
  assert.equal(result.upload[0].existing, undefined);
});

test('empty document list produces no operations', () => {
  const result = planSync([], {}, remote);

  assert.deepEqual(result, { upload: [], skip: [] });
});
