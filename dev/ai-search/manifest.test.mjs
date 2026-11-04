import assert from 'node:assert/strict';
import { mkdir, mkdtemp, readdir, rm } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { test } from 'node:test';
import { createManifest, readManifest, writeManifest } from './manifest.mjs';

test('replaces the manifest and cleans up after a failed rename', async t => {
  const dir = await mkdtemp(join(tmpdir(), 'ai-search-manifest-'));
  t.after(() => rm(dir, { recursive: true, force: true }));
  const path = join(dir, 'manifest.json');
  const manifest = createManifest([{ key: 'index.html', html: '<p>New</p>' }]);

  await writeManifest(path, {});
  await writeManifest(path, manifest);
  assert.deepEqual(await readManifest(path), manifest);

  const blocked = join(dir, 'blocked');
  await mkdir(blocked);
  await assert.rejects(writeManifest(blocked, manifest));
  assert.deepEqual((await readdir(dir)).sort(), ['blocked', 'manifest.json']);
});
