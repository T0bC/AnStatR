// Re-run MathJax typesetting on the help panel whenever its
// content is (re-)rendered by Shiny. MathJax's own automatic
// startup only typesets once on initial page load, but the
// help panel's content is inserted later via renderUI(), so it
// needs to be triggered explicitly on each update.

(function() {
  'use strict';

  function typesetHelpPanel() {
    if (typeof MathJax === 'undefined' || !MathJax.typesetPromise) return;
    var panel = document.getElementById('help-help_panel');
    if (!panel) return;
    MathJax.typesetPromise([panel]).catch(function(err) {
      console.warn('MathJax typeset failed:', err);
    });
  }

  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:value', function(event) {
      if (event.name && event.name.indexOf('help_content') !== -1) {
        setTimeout(typesetHelpPanel, 50);
      }
    });
  }
})();
