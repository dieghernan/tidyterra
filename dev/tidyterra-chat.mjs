const sourceRoot = "https://dieghernan.github.io/tidyterra/dev/";

function makeLink(label, href) {
  const link = document.createElement("a");
  link.textContent = label;
  link.href = href;
  link.target = "_blank";
  link.rel = "noopener noreferrer";
  return link;
}

export function protectLinks(content) {
  const links = [];
  let prefix = "TIDYTERRACHATLINK";
  while (content.includes(prefix)) prefix += "X";

  // Keep code examples out of the link parser. Protect complete links from
  // the widget's emphasis replacements, including underscores in their URLs.
  const text = content.replace(
    /```[\s\S]*?```|`[^`\n]+`|\[([^\]\n]+)\]\((https?:\/\/[^\s)]+)\)/g,
    (match, label, href) => {
      if (!href) return match;
      links.push({ label: label.replaceAll("`", ""), href });
      return prefix + (links.length - 1) + "END";
    }
  );
  return { text, links, prefix };
}

export function observeChatSources(stream, onSources) {
  const decoder = new TextDecoder();
  let pending = "";
  return stream.pipeThrough(new TransformStream({
    transform(bytes, controller) {
      pending += decoder.decode(bytes, { stream: true });
      let separator;
      while ((separator = /\r?\n\r?\n/.exec(pending))) {
        const lines = pending.slice(0, separator.index).split(/\r?\n/);
        pending = pending.slice(separator.index + separator[0].length);
        if (!lines.some(line => /^event: ?chunks$/.test(line))) continue;
        const data = lines.filter(line => line.startsWith("data:"))
          .map(line => line.slice(5).trimStart()).join("\n");
        let chunks;
        try {
          chunks = JSON.parse(data);
        } catch {
          continue;
        }
        if (Array.isArray(chunks)) onSources(chunks);
      }
      controller.enqueue(bytes);
    }
  }));
}

function sourceDisclosure(chunks) {
  const keys = new Set(chunks.map(chunk => chunk?.item?.key).filter(key =>
    typeof key === "string" &&
    /^(?:index\.html|(?:reference|articles)\/[\w.-]+\.html)$/.test(key)
  ));
  if (!keys.size) return null;

  const details = document.createElement("details");
  details.className = "tidyterra-chat-sources";
  const summary = document.createElement("summary");
  summary.textContent = `Sources consulted (${keys.size})`;
  const list = document.createElement("ul");
  for (const key of keys) {
    const item = document.createElement("li");
    const label = key === "index.html" ? "tidyterra overview" : key.replace(/\.html$/, "");
    item.append(makeLink(label, new URL(key, sourceRoot).href));
    list.append(item);
  }
  details.append(summary, list);
  return details;
}

export function enhanceTidyterraChat(chat) {
  // The pinned Cloudflare widget exposes these instance methods. Keep the
  // enhancements scoped to this chat and retain its existing stream parser.
  const view = chat.chatView;
  const client = chat.client;
  if (!view?.renderMessage || !client?.parseChatStream) return;
  const sources = new WeakMap();
  const openSources = new WeakSet();
  const render = view.renderMessage.bind(view);
  const parse = client.parseChatStream.bind(client);

  client.parseChatStream = stream => {
    const message = chat.getMessages().find(item =>
      item.id === view.currentStreamingMessageId
    );
    if (!message) return parse(stream);
    return parse(observeChatSources(stream, chunks => sources.set(message, chunks)));
  };

  view.renderMessage = (message, streaming) => {
    if (streaming || message.role !== "assistant" || !message.content) {
      return render(message, streaming);
    }
    const { text, links, prefix } = protectLinks(message.content);
    const html = render({ ...message, content: text }, streaming)
      .replace(new RegExp(prefix + "(\\d+)END", "g"), (_, index) => {
        const { label, href } = links[index];
        return makeLink(label, href).outerHTML;
      });
    const details = sourceDisclosure(sources.get(message) ?? []);
    if (!details) return html;
    details.open = openSources.has(message);
    const template = document.createElement("template");
    template.innerHTML = html;
    template.content.querySelector(".chat-message-bubble").append(details);
    return template.innerHTML;
  };

  chat.shadowRoot.addEventListener("toggle", event => {
    const details = event.target;
    if (!details.matches(".tidyterra-chat-sources")) return;
    const id = details.closest("[data-message-id]")?.dataset.messageId;
    const message = chat.getMessages().find(item => item.id === id);
    if (!message) return;
    if (details.open) openSources.add(message);
    else openSources.delete(message);
  }, true);
}
