import assert from 'node:assert/strict';
import { readFile } from 'node:fs/promises';
import { test } from 'node:test';
import { observeChatSources, protectMarkdown, sourceRoot } from '../../pkgdown/assets/tidyterra-chat.mjs';

test('links chat sources to the published documentation', () => {
  assert.equal(sourceRoot, 'https://dieghernan.github.io/tidyterra/');
  assert.equal(
    new URL('reference/geom_spatraster.html', sourceRoot).href,
    'https://dieghernan.github.io/tidyterra/reference/geom_spatraster.html'
  );
});

test('system prompt cites the published documentation, not the dev site', async () => {
  const settings = JSON.parse(
    await readFile(new URL('./settings.json', import.meta.url), 'utf8')
  );
  assert.match(settings.system_prompt_ai_search, /https:\/\/dieghernan\.github\.io\/tidyterra\//);
  assert.doesNotMatch(settings.system_prompt_ai_search, /tidyterra\/dev\//);
});

test('AI Search workflow deploys the published docs and repository prompt', async () => {
  const workflow = await readFile(
    new URL('../../.github/workflows/ai-search.yaml', import.meta.url),
    'utf8'
  );
  const workflowRun = workflow.split('  workflow_run:\n')[1]?.split('\n\npermissions:')[0];
  assert.ok(workflowRun);
  assert.doesNotMatch(workflowRun, /branches:/);
  assert.match(workflow, /vars\.AI_SEARCH_AUTO_SYNC == 'true'/);
  assert.match(workflow, /cf ai-search update tidyterra-ai-search --namespace default --system-prompt-ai-search/);
  assert.match(workflow, /if: \$\{\{ github\.event_name != 'workflow_dispatch' \|\| inputs\.mode != 'reconcile' \}\}/);
  assert.ok(workflow.indexOf('      - name: Sync Cloudflare AI Search') < workflow.indexOf('      - name: Update AI Search assistant prompt'));
  assert.match(workflow, /node sync\.mjs site --reconcile/);
  assert.match(workflow, /args=\(site --all-batches\)/);
  assert.doesNotMatch(workflow, /site\/dev|--development/);
});

test('protects underscores in Markdown links without rewriting code examples', () => {
  const url = 'https://example.test/geom_spatraster.html';
  const link = '[`geom_spatraster()`](' + url + ')';
  const { text, tokens, prefix } = protectMarkdown(link + ' and `' + url + '`.');
  assert.equal(text, prefix + '0END and ' + prefix + '1END.');
  assert.deepEqual(tokens, [
    { type: 'link', label: 'geom_spatraster()', href: url },
    { type: 'inline', text: url }
  ]);
  assert.deepEqual(protectMarkdown('```r\n' + link + '\n```').tokens, [
    { type: 'block', text: link }
  ]);
});

test('preserves code underscores, Markdown characters and indentation literally', () => {
  const code = 'r_name <- "\u003cscript\u003e"\n  geom_spatraster(data = r_name) # **literal**';
  const result = protectMarkdown('`r_name`\n\n```r\n' + code + '\n```\n\n_after_');
  assert.equal(result.text, result.prefix + '1END\n\n' + result.prefix + '0END\n\n_after_');
  assert.deepEqual(result.tokens, [
    { type: 'block', text: code },
    { type: 'inline', text: 'r_name' }
  ]);
});

test('observes split SSE sources and forwards the original stream', async () => {
  const chunks = [{ item: { key: 'reference/geom_spatraster.html' } }];
  const sse = 'event: chunks\r\ndata: ' + JSON.stringify(chunks) + '\r\n\r\n' +
    'data: {"choices":[{"delta":{"content":"Café"}}]}\n\ndata: [DONE]\n\n';
  const bytes = new TextEncoder().encode(sse);
  const stream = new ReadableStream({
    start(controller) {
      for (const byte of bytes) controller.enqueue(Uint8Array.of(byte));
      controller.close();
    }
  });
  const found = [];
  const observed = observeChatSources(stream, items => found.push(items));
  assert.equal(await new Response(observed).text(), sse);
  assert.deepEqual(found, [chunks]);
});
