const sourceRoot = "https://dieghernan.github.io/tidyterra/dev/";

function makeLink(label, href) {
  const link = document.createElement("a");
  link.textContent = label;
  link.href = href;
  link.target = "_blank";
  link.rel = "noopener noreferrer";
  return link;
}

export function protectMarkdown(content) {
  const tokens = [];
  let prefix = "TIDYTERRACHATTOKEN";
  while (content.includes(prefix)) prefix += "X";
  const protect = token => {
    tokens.push(token);
    return prefix + (tokens.length - 1) + "END";
  };
  // Extract code before the widget's Markdown substitutions touch its contents.
  const blocks = content.replace(
    /^ {0,3}(`{3,}|~{3,})([^\n]*)\r?\n([\s\S]*?)\r?\n {0,3}\1[ \t]*(?=\r?\n|$)/gm,
    (_, fence, language, code) => protect({ type: "block", text: code })
  );
  const text = blocks.replace(
    /\[([^\]\n]+)\]\((https?:\/\/[^\s)]+)\)|(`+)([^\n]*?[^\`])\3(?!`)/g,
    (match, label, href, delimiter, code) => {
      if (href) return protect({ type: "link", label: label.replaceAll("`", ""), href });
      return protect({ type: "inline", text: code });
    }
  );
  return { text, tokens, prefix };
}

function renderToken(token) {
  if (token.type === "link") return makeLink(token.label, token.href).outerHTML;
  const code = document.createElement("code");
  code.textContent = token.text;
  if (token.type === "inline") return code.outerHTML;
  const block = document.createElement("div");
  block.className = "tidyterra-chat-code";
  const button = document.createElement("button");
  button.type = "button";
  button.className = "tidyterra-copy-code";
  button.textContent = "Copy code";
  button.setAttribute("aria-live", "polite");
  const pre = document.createElement("pre");
  pre.append(code);
  block.append(button, pre);
  return block.outerHTML;
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
  const addExamples = () => {
    const empty = view.messagesContainer?.querySelector(".chat-empty");
    if (!empty || empty.querySelector(".tidyterra-chat-example")) return;
    for (const question of [
      "How do I plot a SpatRaster?",
      "How do I convert SpatVector to sf?",
      "What is geom_spatraster?"
    ]) {
      const button = document.createElement("button");
      button.type = "button";
      button.className = "tidyterra-chat-example";
      button.textContent = question;
      button.addEventListener("click", () => chat.sendMessage(question));
      empty.append(button);
    }
  };
  new MutationObserver(addExamples).observe(view.messagesContainer, { childList: true });
  addExamples();

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
    const { text, tokens, prefix } = protectMarkdown(message.content);
    const html = render({ ...message, content: text }, streaming)
      .replace(new RegExp("<p>(" + prefix + "\\d+END)</p>", "g"), (match, marker) => {
        const index = Number(marker.slice(prefix.length, -3));
        return tokens[index].type === "block" ? marker : match;
      })
      .replace(new RegExp(prefix + "(\\d+)END", "g"), (_, index) => {
        return renderToken(tokens[index]);
      });
    const details = sourceDisclosure(sources.get(message) ?? []);
    if (!details) return html;
    details.open = openSources.has(message);
    const template = document.createElement("template");
    template.innerHTML = html;
    template.content.querySelector(".chat-message-bubble").append(details);
    return template.innerHTML;
  };

  chat.shadowRoot.addEventListener("click", async event => {
    const button = event.target.closest(".tidyterra-copy-code");
    if (!button) return;
    const code = button.closest(".tidyterra-chat-code").querySelector("code").textContent;
    try {
      await navigator.clipboard.writeText(code);
      button.textContent = "Copied";
    } catch {
      button.textContent = "Copy failed";
    }
    setTimeout(() => { button.textContent = "Copy code"; }, 2000);
  });

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
