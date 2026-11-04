
import { createHash, randomUUID } from 'node:crypto';
import { readFile, rename, rm, writeFile } from 'node:fs/promises';

export function hashDocument(html) {
  return createHash('sha256')
    .update(html, 'utf8')
    .digest('hex');
}

export function createManifest(documents) {
  return Object.fromEntries(
    documents.map(({ key, html }) => [
      key,
      hashDocument(html)
    ])
  );
}

export async function readManifest(path) {
  try {
    const content = await readFile(path, 'utf8');
    const manifest = JSON.parse(content);

    if (
      !manifest ||
      Array.isArray(manifest) ||
      typeof manifest !== 'object' ||
      !Object.entries(manifest).every(
        ([key, hash]) =>
          typeof key === 'string' &&
          typeof hash === 'string' &&
          /^[a-f0-9]{64}$/.test(hash)
      )
    ) {
      throw new Error('Invalid AI Search manifest.');
    }

    return manifest;
  } catch (error) {
    if (error.code === 'ENOENT') return null;
    throw error;
  }
}

export async function writeManifest(path, manifest) {
  const temporary = `${path}.${randomUUID()}.tmp`;
  try {
    await writeFile(
      temporary,
      JSON.stringify(manifest, null, 2) + '\n',
      'utf8'
    );
    await rename(temporary, path);
  } finally {
    await rm(temporary, { force: true });
  }
}
