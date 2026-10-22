import { writeFile, mkdir } from 'node:fs/promises';

const endpoint = 'https://ca9c109c-ee53-446c-a7fc-08328b87799b.search.ai.cloudflare.com';
const questions = [
  'How do I facet all layers of a SpatRaster with ggplot2? Give a short example and cite the documentation.',
  '¿Qué diferencia hay entre select() y filter() para un SpatRaster? Cita las fuentes.',
  'Can tidyterra train a neural network? If this is not documented, say so.'
];
await mkdir('preview', { recursive: true });
const results = [];
for (const question of questions) {
  const response = await fetch(`${endpoint}/chat/completions`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ messages: [{ role: 'user', content: question }], stream: false, max_tokens: 700 }),
    signal: AbortSignal.timeout(120000)
  });
  const text = await response.text();
  results.push({ question, status: response.status, body: text });
  await writeFile('preview/smoke.json', JSON.stringify(results, null, 2));
  if (!response.ok) {
    console.error(`Chat unavailable (${response.status}); stopped to avoid further usage. See preview/smoke.json.`);
    process.exitCode = 1;
    break;
  }
  console.log(text);
}
