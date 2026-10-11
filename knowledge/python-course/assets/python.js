/* Python 语言基础与应用 · 知识点课程页 · 共享脚本
   来源：原课程站 assets/course.js（逐字迁移，行为不变）。 */
document.addEventListener('DOMContentLoaded', function () {
  // 知识点地图折叠
  document.querySelectorAll('.kmap-btn').forEach(function (btn) {
    btn.addEventListener('click', function () {
      btn.closest('.kmap').classList.toggle('open');
    });
  });
  // 信息卡点击揭示
  document.querySelectorAll('.icard[data-toggle]').forEach(function (card) {
    card.addEventListener('click', function () { card.classList.toggle('open'); });
  });
  // 分组切换按钮
  document.querySelectorAll('.switch').forEach(function (sw) {
    sw.querySelectorAll('button').forEach(function (b) {
      b.addEventListener('click', function () {
        sw.querySelectorAll('button').forEach(function (x) { x.classList.remove('on'); });
        b.classList.add('on');
        var scope = sw.parentElement;
        scope.querySelectorAll('.pane').forEach(function (p) {
          p.classList.toggle('on', p.id === b.dataset.pane);
        });
      });
    });
  });
});

/* 单选测验：容器 .quiz 上放 data-answer="正确项下标(从0起)" data-ok="" data-no="" */
function quizPick(btn) {
  var quiz = btn.closest('.quiz');
  var opts = Array.prototype.slice.call(quiz.querySelectorAll('.opt'));
  var correct = parseInt(quiz.dataset.answer, 10);
  var idx = opts.indexOf(btn);
  var fb = quiz.querySelector('.quiz-fb');
  opts.forEach(function (b) { b.disabled = true; b.classList.remove('right', 'wrong'); });
  opts[correct].classList.add('right');
  if (idx === correct) {
    fb.textContent = quiz.dataset.ok || '✅ 答对了！';
    fb.className = 'quiz-fb ok';
  } else {
    btn.classList.add('wrong');
    fb.textContent = quiz.dataset.no || '❌ 不对哦，翻上去再看看相关内容。';
    fb.className = 'quiz-fb no';
  }
}

/* 填空：容器 .quiz 上放 data-answer="答案1|答案2"（比较时忽略大小写与空格） */
function checkFill(btn) {
  var box = btn.closest('.quiz');
  var input = box.querySelector('.blank');
  var fb = box.querySelector('.fill-fb');
  var norm = function (s) { return (s || '').toLowerCase().replace(/\s+/g, '').replace(/，/g, ','); };
  var ok = (box.dataset.answer || '').split('|').some(function (a) { return norm(a) === norm(input.value); });
  if (ok) {
    input.classList.remove('wrong'); input.classList.add('right');
    fb.textContent = box.dataset.ok || '✅ 正确！';
    fb.style.color = 'var(--ok)';
  } else {
    input.classList.remove('right'); input.classList.add('wrong');
    fb.textContent = box.dataset.no || '❌ 再想想，可参考上文的内容或表格。';
    fb.style.color = 'var(--err)';
  }
}

/* 复制提示词：.prompt-box 上放 data-text="要复制的完整文本" */
function copyText(btn) {
  var box = btn.closest('.prompt-box');
  var text = box.dataset.text || box.textContent.trim();
  navigator.clipboard.writeText(text).then(function () {
    btn.textContent = '已复制 ✓';
    btn.classList.add('done');
    setTimeout(function () { btn.textContent = '复制'; btn.classList.remove('done'); }, 2000);
  });
}
