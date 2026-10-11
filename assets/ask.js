/* 首页问答组件。后端地址填在这里,密钥不经过浏览器。 */
(function () {
  var WORKER_URL = 'https://jefeerzhang-ask.jefeerzhang.workers.dev/api/ask'; // 线上 Worker 地址
  var LOCAL_WORKER_URL = 'http://127.0.0.1:8787/api/ask'; // npx wrangler dev 默认端口
  var MAX_HISTORY = 6; // 每次请求带上最近 6 条消息

  var isLocal = /^(localhost|127\.0\.0\.1|\[::1\])$/.test(location.hostname);
  var ENDPOINT = isLocal ? LOCAL_WORKER_URL : WORKER_URL;

  var log = document.getElementById('ask-log');
  var form = document.getElementById('ask-form');
  var input = document.getElementById('ask-input');
  var send = document.getElementById('ask-send');
  var note = document.getElementById('ask-note');
  if (!log || !form || !input || !send) return;

  var history = [];
  var busy = false;

  function bubble(role, text) {
    var wrap = document.createElement('div');
    wrap.className = 'ask-msg ask-msg--' + role;
    var name = document.createElement('span');
    name.className = 'ask-role';
    name.textContent = role === 'user' ? '你' : '张老师的 agent';
    // div 而不是 p:回答里可能有 ul / pre 这类块级元素,p 装不进
    var body = document.createElement('div');
    body.className = 'ask-bubble';
    body.textContent = text || '';
    wrap.appendChild(name);
    wrap.appendChild(body);
    log.appendChild(wrap);
    log.scrollTop = log.scrollHeight;
    return body;
  }

  function say(role, text, isError) {
    var body = bubble(role, text);
    if (isError) body.className = 'ask-bubble ask-bubble--error';
    return body;
  }

  if (!ENDPOINT) {
    log.innerHTML = '';
    say('bot', '问答后端还没有配置。请先在 assets/ask.js 的 WORKER_URL 里填写地址。', true);
    send.disabled = true;
    input.disabled = true;
    return;
  }

  // MiniMax 的思考内容可能以 <think> 标签混在正文里,显示前去掉
  function stripThink(s) {
    var out = s.replace(/<(think|thinking)>[\s\S]*?<\/(think|thinking)>/g, '');
    var cut = out.search(/<(think|thinking)>/);
    return cut === -1 ? out : out.slice(0, cut);
  }

  function esc(s) {
    return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  }

  // 只渲染回答里常见的一小撮 Markdown。先转义再替换,所以模型输出的 HTML 不会生效。
  function renderMarkdown(src) {
    var store = [];
    function stash(html) {
      store.push(html);
      return '\u0000' + (store.length - 1) + '\u0000';
    }

    var s = esc(src);

    s = s.replace(/```[^\n]*\n?([\s\S]*?)```/g, function (m, code) {
      return stash('<pre class="md-pre"><code>' + code.replace(/\n+$/, '') + '</code></pre>');
    });
    s = s.replace(/`([^`\n]+)`/g, function (m, code) {
      return stash('<code class="md-code">' + code + '</code>');
    });
    s = s.replace(/\[([^\]\n]+)\]\((https?:\/\/[^\s)]+)\)/g, function (m, text, url) {
      return stash('<a href="' + url + '" target="_blank" rel="noopener noreferrer">' + text + '</a>');
    });
    s = s.replace(/\*\*([^*\n]+)\*\*/g, '<strong>$1</strong>');
    s = s.replace(/__([^_\n]+)__/g, '<strong>$1</strong>');
    s = s.replace(/(^|[^*\w])\*([^*\n]+)\*(?![*\w])/g, '$1<em>$2</em>');
    // 模型常直接贴裸 URL,不给 Markdown 语法,这里补成链接
    s = s.replace(/https?:\/\/[^\s<>"'()\[\]{}）】，。；：！？]+/g, function (url) {
      var tail = '';
      var m = /[.,;:!?]+$/.exec(url);
      if (m) {
        tail = m[0];
        url = url.slice(0, -m[0].length);
      }
      return stash('<a href="' + url + '" target="_blank" rel="noopener noreferrer">' + url + '</a>') + tail;
    });

    var lines = s.split('\n');
    var html = [];
    var para = [];
    var listTag = null;

    function flushPara() {
      if (para.length) {
        html.push('<p>' + para.join('<br>') + '</p>');
        para = [];
      }
    }
    function closeList() {
      if (listTag) {
        html.push('</' + listTag + '>');
        listTag = null;
      }
    }

    for (var i = 0; i < lines.length; i++) {
      var line = lines[i];
      var t = line.trim();

      if (/^\u0000\d+\u0000$/.test(t)) {
        flushPara();
        closeList();
        html.push(t);
        continue;
      }
      if (!t) {
        flushPara();
        closeList();
        continue;
      }
      var h = /^(#{1,6})\s+(.*)$/.exec(t);
      if (h) {
        flushPara();
        closeList();
        var lvl = Math.min(h[1].length + 2, 6);
        html.push('<h' + lvl + '>' + h[2] + '</h' + lvl + '>');
        continue;
      }
      var ul = /^[-*+]\s+(.*)$/.exec(t);
      if (ul) {
        flushPara();
        if (listTag !== 'ul') {
          closeList();
          html.push('<ul>');
          listTag = 'ul';
        }
        html.push('<li>' + ul[1] + '</li>');
        continue;
      }
      var ol = /^\d+[.)]\s+(.*)$/.exec(t);
      if (ol) {
        flushPara();
        if (listTag !== 'ol') {
          closeList();
          html.push('<ol>');
          listTag = 'ol';
        }
        html.push('<li>' + ol[1] + '</li>');
        continue;
      }
      closeList();
      para.push(line);
    }
    flushPara();
    closeList();

    return html.join('').replace(/\u0000(\d+)\u0000/g, function (m, n) {
      return store[+n];
    });
  }

  // 流式过程中落单的标记先藏起来,免得闪出 ** 或半个链接
  function trimDangling(s) {
    if ((s.match(/\*\*/g) || []).length % 2) {
      var i = s.lastIndexOf('**');
      if (i !== -1) s = s.slice(0, i);
    }
    if ((s.match(/`/g) || []).length % 2) {
      var j = s.lastIndexOf('`');
      if (j !== -1) s = s.slice(0, j);
    }
    var open = s.lastIndexOf('[');
    if (open !== -1 && s.slice(open).indexOf(']') === -1) s = s.slice(0, open);
    return s;
  }

  // 识别上游增量:MiniMax Responses 事件优先,兼容 OpenAI Chat 分片
  function extractPiece(e) {
    if (typeof e.delta === 'string') {
      var type = e.type || '';
      return type.indexOf('reasoning') === -1 ? e.delta : '';
    }
    var delta = e.choices && e.choices[0] && e.choices[0].delta;
    return delta && typeof delta.content === 'string' ? delta.content : '';
  }

  // 增量事件没认出来时的兜底:完成事件里带完整正文
  function wholeText(e) {
    var r = e.response;
    if (r && typeof r.output_text === 'string' && r.output_text) return r.output_text;
    return '';
  }

  function failureText(e) {
    if (e.type === 'response.failed' && e.response && e.response.error) {
      return e.response.error.message || '模型返回失败。';
    }
    if (e.error) {
      return typeof e.error === 'string' ? e.error : e.error.message || '模型返回错误。';
    }
    return '';
  }

  form.addEventListener('submit', function (event) {
    event.preventDefault();
    ask();
  });

  input.addEventListener('keydown', function (event) {
    if (event.key === 'Enter' && !event.shiftKey && !event.isComposing) {
      event.preventDefault();
      ask();
    }
  });

  async function ask() {
    if (busy) return;
    var question = input.value.trim();
    if (!question) return;

    busy = true;
    send.disabled = true;
    input.value = '';
    say('user', question);

    var answer = bubble('bot', '');
    answer.classList.add('ask-bubble--md');
    var caret = document.createElement('span');
    caret.className = 'ask-typing';
    answer.appendChild(caret);

    var text = '';
    try {
      var res = await fetch(ENDPOINT, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ question: question, history: history })
      });

      if (!res.ok) {
        var detail = '';
        try {
          detail = (await res.json()).error || '';
        } catch (e) {
          detail = '';
        }
        caret.remove();
        answer.className = 'ask-bubble ask-bubble--error';
        answer.textContent = detail || '服务暂时不可用，请稍后再试。';
        return;
      }

      var reader = res.body.getReader();
      var decoder = new TextDecoder();
      var buffer = '';
      var whole = '';
      var failed = '';

      while (true) {
        var chunk = await reader.read();
        if (chunk.done) break;
        buffer += decoder.decode(chunk.value, { stream: true });
        var lines = buffer.split('\n');
        buffer = lines.pop();
        for (var i = 0; i < lines.length; i++) {
          var line = lines[i].trim();
          if (line.indexOf('data:') !== 0) continue;
          var payload = line.slice(5).trim();
          if (!payload || payload === '[DONE]') continue;
          var evt;
          try {
            evt = JSON.parse(payload);
          } catch (e) {
            continue;
          }
          failed = failureText(evt) || failed;
          whole = wholeText(evt) || whole;
          text += extractPiece(evt);
          answer.innerHTML = renderMarkdown(trimDangling(stripThink(text || whole)));
          answer.appendChild(caret);
          log.scrollTop = log.scrollHeight;
        }
      }

      caret.remove();
      var reply = stripThink(text || whole);
      if (reply) answer.innerHTML = renderMarkdown(reply);
      if (!reply) {
        answer.className = 'ask-bubble ask-bubble--error';
        answer.textContent = failed || '没有收到回答，请再试一次。';
        return;
      }
      history.push({ role: 'user', content: question });
      history.push({ role: 'assistant', content: reply });
      if (history.length > MAX_HISTORY) history = history.slice(-MAX_HISTORY);
    } catch (e) {
      caret.remove();
      answer.className = 'ask-bubble ask-bubble--error';
      answer.textContent = isLocal
        ? '连不上本地问答后端。请确认 npx wrangler dev 正在运行。'
        : '网络错误，请稍后再试。';
    } finally {
      busy = false;
      send.disabled = false;
      input.focus();
      if (note) note.textContent = '回答由大模型生成，可能出错。';
    }
  }
})();
