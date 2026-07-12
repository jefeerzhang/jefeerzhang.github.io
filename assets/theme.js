/* Dark mode toggle
   - 立即执行: 在 <head> 顶部读取 localStorage, 挂 data-theme 防止 FOUC
   - 暴露 window.Theme.toggle / window.Theme.current
   - 写入 localStorage 'site-theme'
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
    // 通知页面按钮更新 label
    document.dispatchEvent(new CustomEvent('site:theme', { detail: { theme: theme } }));
  }

  // 立即执行 (在 head 顶部, 不等 DOMContentLoaded)
  apply(read());

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
    }
  };
})();