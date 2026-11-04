import assert from 'node:assert/strict';
import test from 'node:test';
import { load } from 'cheerio';
import { cleanPage, includePage } from './content.mjs';

test('only one pkgdown version and documentation sections are indexed', () => {
  for (const path of ['index.html', 'reference/filter.html', 'articles/faqs.html']) assert.ok(includePage(path));
  for (const path of ['dev/index.html', 'BS5/index.html', '404.html', 'news/index.html']) assert.equal(includePage(path), false);
});

test('index keeps R examples, removes chrome and resolves links for citations', () => {
  const html = '<title>Filter • tidyterra</title><nav>Navigation</nav><main id="main"><h1>Filter</h1><pre><code>x |&gt; filter(a &gt; 1)</code></pre><a href="../articles/faqs.html#raster">FAQ</a><script>bad()</script></main><aside>Sidebar</aside>';
  const $ = load(cleanPage(html, 'reference/filter.html'));
  assert.equal($('code').text(), 'x |> filter(a > 1)');
  assert.equal($('a').attr('href'), 'https://dieghernan.github.io/tidyterra/articles/faqs.html#raster');
  assert.equal($('link[rel="canonical"]').attr('href'), 'https://dieghernan.github.io/tidyterra/reference/filter.html');
  assert.equal($('script, nav, aside').length, 0);
  assert.match($('body').text(), /Source: https:/);
});

test('redirects are skipped and missing content fails before upload', () => {
  assert.equal(cleanPage('<meta http-equiv="refresh" content="0;url=x">', 'index.html'), null);
  assert.throws(() => cleanPage('<p>Wrong layout</p>', 'index.html'), /main#main/);
});

test('explicit development mode includes pkgdown noindex pages and uses dev URLs', () => {
  const html = '<meta name="robots" content="noindex"><main id="main"><a href="other.html">Other</a></main>';
  const key = 'reference/example.html';
  const options = { development: true };
  assert.equal(cleanPage(html, key), null);
  const $ = load(cleanPage(html, key, options));
  assert.equal($('link[rel="canonical"]').attr('href'), 'https://dieghernan.github.io/tidyterra/dev/reference/example.html');
  assert.equal($('a').attr('href'), 'https://dieghernan.github.io/tidyterra/dev/reference/other.html');
  assert.equal(cleanPage('<meta http-equiv="refresh" content="0;url=x">', key, options), null);
});
