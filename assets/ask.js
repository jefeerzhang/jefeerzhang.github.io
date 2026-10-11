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
    name.textContent = role === 'user' ? '你' : '助手';
    var body = document.createElement('p');
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
          answer.textContent = stripThink(text || whole);
          answer.appendChild(caret);
          log.scrollTop = log.scrollHeight;
        }
      }

      caret.remove();
      var reply = stripThink(text || whole);
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
