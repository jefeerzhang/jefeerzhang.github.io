document.querySelectorAll('[data-quiz]').forEach((quiz) => {
  const feedback = quiz.querySelector('.feedback');
  quiz.querySelectorAll('button[data-answer]').forEach((button) => {
    button.addEventListener('click', () => {
      const isCorrect = button.dataset.answer === 'correct';
      quiz.querySelectorAll('button[data-answer]').forEach((item) => {
        item.disabled = true;
        if (item.dataset.answer === 'correct') item.classList.add('correct');
      });
      if (!isCorrect) button.classList.add('wrong');
      feedback.textContent = isCorrect ? quiz.dataset.correct : quiz.dataset.wrong;
      feedback.className = `feedback ${isCorrect ? 'correct' : 'wrong'}`;
    });
  });
});
