import assert from 'node:assert/strict';
import { test } from 'node:test';
import { observeChatSources, protectLinks } from '../../pkgdown/assets/tidyterra-chat.mjs';

test('protects underscores in Markdown links without rewriting code examples', () => {
  const url = 'https://example.test/geom_spatraster.html';
  const link = '[`geom_spatraster()`](' + url + ')';
  const { text, links, prefix } = protectLinks(link + ' and `' + url + '`.');
  assert.equal(text, prefix + '0END and `' + url + '`.');
  assert.deepEqual(links, [{ label: 'geom_spatraster()', href: url }]);
  assert.deepEqual(protectLinks('```r\n' + link + '\n```').links, []);
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
