
import { planSync } from './plan.mjs';
import { uploadDocument } from './items.mjs';
import { writeManifest } from './manifest.mjs';
import { includePage } from './content.mjs';

export async function executeSync({
  cf,
  documents,
  current,
  previous,
  existing,
  confirmedThisRun = new Set(),
  manifestPath,
  prune = false,
  upload = uploadDocument,
  save = writeManifest,
  log = console.log,
  maxUploads = Infinity
}) {
  const plan = planSync(
    documents.map(doc => ({
      ...doc,
      hash: current[doc.key]
    })),
    previous,
    existing,
    confirmedThisRun
  );

  const state = { ...(previous ?? {}) };
  let uploaded = 0;
  let deleted = 0;

  log(
    `Sync plan: ${plan.upload.length} uploads, ` +
    `${plan.skip.length} unchanged.`
  );

  for (const document of plan.upload.slice(0, maxUploads)) {
    // Remove the trusted hash before any remote mutation.
    delete state[document.key];
    await save(manifestPath, state);

    await upload(cf, document, document.existing);

    state[document.key] = current[document.key];
    await save(manifestPath, state);
    confirmedThisRun.add(document.key);

    uploaded++;
    log(`Uploaded ${document.key}`);
  }

  const pending = plan.upload.length - uploaded;

  if (pending > 0) {
    log(
      `${pending} documents remain pending. ` +
      'Pruning deferred until uploads are complete.'
    );
  }

  if (prune && pending === 0) {
    const wanted = new Set(documents.map(doc => doc.key));

    for (const item of existing) {
      if (
        item.source_id === 'builtin' &&
        item.id &&
        includePage(item.key) &&
        !wanted.has(item.key)
      ) {
        await cf(`/items/${encodeURIComponent(item.id)}`, {
          method: 'DELETE'
        });

        delete state[item.key];
        await save(manifestPath, state);

        deleted++;
        log(`Deleted stale document ${item.key}`);
      }
    }
  }

  for (const key of Object.keys(state)) {
    if (!Object.hasOwn(current, key)) {
      delete state[key];
    }
  }

  await save(manifestPath, state);

  return {
    uploaded,
    skipped: plan.skip.length,
    deleted,
    pending
  };
}
