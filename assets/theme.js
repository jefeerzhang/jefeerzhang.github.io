/* Dark mode toggle
   - 立即执行: 在 <head> 顶部读取 localStorage, 挂 data-theme 防止 FOUC
   - 自动注入切换按钮 (fixed 浮窗), 即使原 HTML 没有 nav 也能切
   - 暴露 window.Theme.toggle / set / current
*/
(function () {
  'use strict';
  var KEY = 'site-theme';
  var LIGHT = 'light';
  var DARK = 'dark';

  function read() {
    try {
      var v = localStorage.getItem(KEY);
      if (v === DARK || v === LIGHT) return v;
    } catch (e) {}
    return LIGHT;
  }

  function apply(theme) {
    var html = document.documentElement;
    if (theme === DARK) {
      html.setAttribute('data-theme', 'dark');
    } else {
      html.removeAttribute('data-theme');
    }
    // 通知页面已注册的 listener (例如 index.html 的主 nav 按钮)
    try { document.dispatchEvent(new CustomEvent('site:theme', { detail: { theme: theme } })); } catch (e) {}
  }

  // 立即执行 (在 head 顶部, 不等 DOMContentLoaded)
  apply(read());

  function injectFab() {
    if (document.getElementById('theme-fab')) return;
    // 如果页面已经有 #theme-toggle (主 nav 内嵌), 就不重复注入
    if (document.getElementById('theme-toggle')) return;
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.id = 'theme-fab';
    btn.className = 'theme-toggle theme-toggle--fab';
    btn.setAttribute('aria-label', '切换主题');
    btn.innerHTML =
      '<svg class="icon-moon" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M21 12.8A9 9 0 1 1 11.2 3a7 7 0 0 0 9.8 9.8z"/></svg>' +
      '<svg class="icon-sun" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><circle cx="12" cy="12" r="4"/><path d="M12 2v2"/><path d="M12 20v2"/><path d="M4.9 4.9l1.4 1.4"/><path d="M17.7 17.7l1.4 1.4"/><path d="M2 12h2"/><path d="M20 12h2"/><path d="M4.9 19.1l1.4-1.4"/><path d="M17.7 6.3l1.4-1.4"/></svg>';
    btn.addEventListener('click', function () { window.Theme.toggle(); });
    document.body.appendChild(btn);
  }

  window.Theme = {
    get current() { return read(); },
    set: function (t) {
      try { localStorage.setItem(KEY, t); } catch (e) {}
      apply(t);
    },
    toggle: function () {
      var next = read() === DARK ? LIGHT : DARK;
      try { localStorage.setItem(KEY, next); } catch (e) {}
      apply(next);
      return next;
    },
    injectFab: injectFab
  };

  // DOM 就绪后注入 FAB (浮窗), 让没有 nav 的页面 (例如课节) 也能切
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', injectFab);
  } else {
    injectFab();
  }
})();