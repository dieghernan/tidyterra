import { setTimeout as delay } from 'node:timers/promises';

// Reads and deletes can be retried safely. Uploads are not retried because a
// duplicate-key response could conceal an upload that already succeeded.
export function createCloudflareClient(base, token, {
  fetchImpl = fetch, sleep = delay, now = Date.now, log = console.warn
} = {}) {
  return async function cf(path, options = {}) {
    const method = options.method || 'GET';
    let waitedMs = 0;
    let retryingDelete = false;
    for (let attempt = 0; ; attempt++) {
      const response = await fetchImpl(base + path, {
        ...options,
        headers: { Authorization: `Bearer ${token}`, ...options.headers },
        signal: AbortSignal.timeout(120000)
      });
      if (response.status === 404 && retryingDelete) {
        return { success: true, result: null };
      }
      if (response.status === 429 && ['GET', 'DELETE'].includes(method) && attempt < 5) {
        const header = response.headers.get('Retry-After');
        const seconds = header?.trim() ? Number(header) : NaN;
        const date = header ? Date.parse(header) : NaN;
        const requestedMs = Number.isFinite(seconds) && seconds >= 0 ? seconds * 1000
          : Number.isFinite(date) ? date - now() : 5000 * 2 ** attempt;
        const waitMs = Math.max(1000, requestedMs);
        // Do not ignore a long Retry-After by retrying before it expires.
        if (waitedMs + waitMs <= 300000) {
          await response.body?.cancel();
          log(`Cloudflare throttled ${method} ${path}; retrying in ${waitMs / 1000}s (${attempt + 1}/5).`);
          await sleep(waitMs);
          waitedMs += waitMs;
          retryingDelete = method === 'DELETE';
          continue;
        }
      }
      // Avoid logging credentials or server payloads in Actions, including
      // overload responses that contain HTML instead of JSON.
      if (!response.ok) throw new Error(`Cloudflare request failed: ${method} ${path} (${response.status})`);
      const body = await response.json();
      if (body?.success === false) throw new Error(`Cloudflare request failed: ${method} ${path} (${response.status})`);
      return body;
    }
  };
}
