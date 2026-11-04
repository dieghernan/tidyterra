
export function planSync(
  documents,
  previous,
  remoteItems,
  confirmedThisRun = new Set()
) {
  const remote = new Map(
    remoteItems
      .filter(item => item.source_id === 'builtin')
      .map(item => [item.key, item])
  );

  const upload = [];
  const skip = [];

  for (const document of documents) {
    const item = remote.get(document.key);
    const confirmed = item?.id || confirmedThisRun.has(document.key);

    if (confirmed && previous?.[document.key] === document.hash) {
      skip.push(document.key);
    } else {
      upload.push({
        ...document,
        existing: item
      });
    }
  }

  return { upload, skip };
}
