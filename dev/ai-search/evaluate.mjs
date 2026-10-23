import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { fileURLToPath } from 'node:url';
import { join } from 'node:path';

const directory = fileURLToPath(new URL('.', import.meta.url));
const cases = JSON.parse(await readFile(join(directory, 'evaluation-cases.json'), 'utf8'));
const selected = process.argv.slice(2);
const unknown = selected.filter(id => !cases.some(item => item.id === id));
if (unknown.length) throw new Error(`Unknown evaluation cases: ${unknown.join(', ')}`);
const endpoint = 'https://ca9c109c-ee53-446c-a7fc-08328b87799b.search.ai.cloudflare.com';
const output = join(directory, 'evaluations', new Date().toISOString().replaceAll(':', '-'));
await mkdir(output, { recursive: true });
const results = [];
for (const item of cases.filter(item => !selected.length || selected.includes(item.id))) {
  const messages = [{ role: 'user', content: item.question }];
  if (item.followupTo) {
    const previous = results.find(result => result.id === item.followupTo);
    if (!previous?.answer) throw new Error(`Run ${item.followupTo} before ${item.id}.`);
    messages.unshift({ role: 'user', content: previous.question }, { role: 'assistant', content: previous.answer });
  }
  const started = Date.now();
  const response = await fetch(`${endpoint}/chat/completions`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ messages, stream: false, max_tokens: 1200,
      ai_search_options: { cache: { enabled: false }, query_rewrite: { enabled: false } } }),
    signal: AbortSignal.timeout(120000)
  });
  const text = await response.text();
  let raw;
  try { raw = JSON.parse(text); } catch { raw = { text }; }
  const body = raw.result ?? raw;
  const result = { ...item, status: response.status, elapsedMs: Date.now() - started,
    answer: body.choices?.[0]?.message?.content ?? body.response ?? null,
    finishReason: body.choices?.[0]?.finish_reason ?? null, usage: body.usage ?? null, raw };
  results.push(result);
  await writeFile(join(output, `${item.id}.json`), JSON.stringify(result, null, 2));
  console.log(JSON.stringify({ id: item.id, status: result.status, elapsedMs: result.elapsedMs,
    usage: result.usage, finishReason: result.finishReason, answer: result.answer }));
  if (!response.ok || !result.answer) {
    process.exitCode = 1;
    break;
  }
}
console.log(`Evidence: ${output}`);
