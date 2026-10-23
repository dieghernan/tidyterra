import { load } from 'cheerio';

export const SITE = 'https://dieghernan.github.io/tidyterra/';

export function includePage(key) {
  return key === 'index.html' || /^(reference|articles)\/[^/]+\.html$/.test(key);
}

export function cleanPage(html, key) {
  const url = new URL(key, SITE).href;
  const $ = load(html);
  if ($('meta[http-equiv="refresh" i]').length ||
      /noindex/i.test($('meta[name="robots"]').attr('content') || '')) return null;
  const main = $('main#main');
  if (main.length !== 1) throw new Error(`Expected one main#main in ${key}`);
  main.find('script, style, noscript, nav, .anchor, .copy-button, chat-bubble-snippet').remove();
  main.find('a[href], img[src]').each((_, element) => {
    const attr = element.tagName === 'a' ? 'href' : 'src';
    try {
      const target = new URL($(element).attr(attr), url);
      if (['http:', 'https:'].includes(target.protocol)) $(element).attr(attr, target.href);
      else $(element).removeAttr(attr);
    } catch { $(element).removeAttr(attr); }
  });
  const title = $('title').text();
  const output = load('<!doctype html><html lang="en"><head><meta charset="utf-8"></head><body></body></html>');
  output('head').append(output('<title>').text(title));
  output('head').append(output('<link rel="canonical">').attr('href', url));
  output('body').append(output('<p>').text(`Source: ${url}`));
  output('body').append(main);
  return output.html();
}
