import { readdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';

const root = fileURLToPath(new URL('evaluations/', import.meta.url));
const records = [];
for (const directory of await readdir(root, { withFileTypes: true })) {
  if (!directory.isDirectory()) continue;
  for (const file of await readdir(join(root, directory.name))) {
    if (file.endsWith('.json')) records.push(JSON.parse(await readFile(join(root, directory.name, file), 'utf8')));
  }
}
const urls = [...new Set(records.flatMap(record =>
  [...record.answer.matchAll(/https?:\/\/[^\s<>\)\]\"`]+/g)].map(match => match[0].replace(/[.,]+$/, ''))
))];
const links = await Promise.all(urls.map(async url => {
  try {
    const response = await fetch(url, { method: 'HEAD', signal: AbortSignal.timeout(30000) });
    return { url, status: response.status, finalUrl: response.url };
  } catch (error) { return { url, error: error.message }; }
}));
const times = records.map(record => record.elapsedMs).sort((a, b) => a - b);
const summary = {
  requests: records.length,
  httpSuccesses: records.filter(record => record.status === 200).length,
  totalTokens: records.reduce((sum, record) => sum + record.usage.total_tokens, 0),
  reportedNeurons: records.reduce((sum, record) => sum + record.usage.neurons, 0),
  totalElapsedMs: times.reduce((sum, value) => sum + value, 0),
  medianElapsedMs: times.length % 2 ? times[Math.floor(times.length / 2)] : (times[times.length / 2 - 1] + times[times.length / 2]) / 2,
  minElapsedMs: times[0], maxElapsedMs: times.at(-1), links
};
await writeFile(join(root, 'summary.json'), JSON.stringify(summary, null, 2));
console.log(JSON.stringify(summary, null, 2));
