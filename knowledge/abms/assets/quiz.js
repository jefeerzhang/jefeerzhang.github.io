/* ABMS Quiz Widget — shared JS component */
/* Usage: <div class="quiz" data-quiz="quiz1"> ... quiz-options ... </div> */

document.addEventListener('DOMContentLoaded', function() {
  document.querySelectorAll('.quiz').forEach(function(quiz) {
    var options = quiz.querySelectorAll('.quiz-option');
    var feedback = quiz.querySelector('.quiz-feedback');
    var answered = false;

    options.forEach(function(opt) {
      opt.addEventListener('click', function() {
        if (answered) return;
        answered = true;

        var correct = opt.dataset.correct === 'true';

        if (correct) {
          opt.classList.add('correct');
          feedback.classList.add('show', 'success');
          feedback.textContent = feedback.dataset.success || '正确！';
        } else {
          opt.classList.add('wrong');
          // highlight correct answer
          options.forEach(function(o) {
            if (o.dataset.correct === 'true') o.classList.add('correct');
          });
          feedback.classList.add('show', 'fail');
          feedback.textContent = feedback.dataset.fail || '再想想——正确答案已标出。';
        }

        // allow retry after 2s
        setTimeout(function() {
          answered = false;
          options.forEach(function(o) { o.classList.remove('correct', 'wrong'); });
          feedback.classList.remove('show', 'success', 'fail');
        }, 3000);
      });
    });
  });
});
