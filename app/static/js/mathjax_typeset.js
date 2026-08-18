// Re-run MathJax typesetting whenever the help panel's content
// is (re-)rendered by Shiny. MathJax's own automatic startup
// only typesets once on initial page load, but the help panel
// starts empty and is filled later via renderUI(), so a fresh
// typeset pass has to be triggered explicitly whenever its
// content changes. A MutationObserver is used instead of
// listening for a specific Shiny event, since it doesn't
// depend on knowing Shiny's internal event/name format and
// reliably fires for any DOM update regardless of cause.
//
// The details.md files use collapsible <details> blocks
// extensively, nested several levels inside the panel (offcanvas
// body > tab pane > .help-section-content > ... > <details>). A
// *closed* <details> renders its content as the browser-native
// equivalent of display:none — not merely hidden, but genuinely
// unlaid-out. MathJax's typesetting needs to measure the math it
// renders, and per MathJax's own documented guidance (see
// mathjax-docs wiki, "Dealing with display:none"), it cannot do
// that inside display:none content — confirmed here too:
// MathJax's findMath() throws on a container whose subtree
// reaches into a collapsed <details>, and stops throwing once
// every <details> is manually expanded first. The fix follows
// MathJax's own recommended pattern: skip collapsed sections on
// the initial pass, and typeset each <details>'s content only
// once the user opens it (via the native "toggle" event), when
// its content is actually laid out.

(function() {
  'use strict';

  var typesetTimer = null;
  var isTypesetting = false;
  var wiredDetailsEls = new WeakSet();

  function typesetOne(el, retriesLeft) {
    if (typeof MathJax === 'undefined' || !MathJax.typesetPromise) {
      // MathJax's own startup can still be in progress even
      // though its script has loaded (it parses/initialises
      // asynchronously) — retry briefly instead of giving up.
      if (retriesLeft > 0) {
        setTimeout(function() {
          typesetOne(el, retriesLeft - 1);
        }, 200);
      } else {
        console.warn('MathJax did not become ready in time.');
      }
      return Promise.resolve();
    }

    // MathJax keeps internal per-element bookkeeping from the
    // last typeset pass; calling typesetPromise() again on an
    // element it has already processed can throw once the DOM
    // underneath has since been replaced by a new Shiny render.
    // typesetClear() discards that stale state so this pass
    // starts fresh.
    if (MathJax.typesetClear) MathJax.typesetClear([el]);
    return MathJax.typesetPromise([el]).catch(function(err) {
      console.warn('MathJax typeset failed for one section:', err);
    });
  }

  // Collects every element under `root` that should be handed
  // to MathJax as its own separate typeset target: `root` itself
  // if it contains no <details> anywhere below it (the common,
  // fast case), otherwise each of `root`'s children individually
  // (recursing into any child that itself still contains a
  // <details> further down), and each <details> element found is
  // recorded separately rather than included in a container pass.
  function collectTypesetTargets(root, targets, detailsFound) {
    if (root.tagName === 'DETAILS') {
      detailsFound.push(root);
      return;
    }
    if (!root.querySelector('details')) {
      targets.push(root);
      return;
    }
    Array.prototype.forEach.call(root.children, function(child) {
      collectTypesetTargets(child, targets, detailsFound);
    });
  }

  function onDetailsToggle(event) {
    var details = event.target;
    if (!details.open) return;
    isTypesetting = true;
    typesetOne(details, 10).finally(function() {
      isTypesetting = false;
    });
  }

  function wireDetails(detailsEls) {
    detailsEls.forEach(function(d) {
      if (wiredDetailsEls.has(d)) return;
      wiredDetailsEls.add(d);
      d.addEventListener('toggle', onDetailsToggle);
      // Already open when first seen (e.g. re-rendered while
      // expanded) — typeset it immediately since display:none
      // does not apply to an open <details>.
      if (d.open) {
        isTypesetting = true;
        typesetOne(d, 10).finally(function() {
          isTypesetting = false;
        });
      }
    });
  }

  function typesetHelpPanel(panel) {
    // isTypesetting guards against the MutationObserver
    // re-triggering on MathJax's own SVG output being written
    // into the panel, which would otherwise clear/retypeset in
    // an infinite loop.
    isTypesetting = true;

    var targets = [];
    var detailsEls = [];
    collectTypesetTargets(panel, targets, detailsEls);

    var tasks = targets.map(function(t) {
      return typesetOne(t, 10);
    });

    Promise.all(tasks).finally(function() {
      isTypesetting = false;
    });

    wireDetails(detailsEls);
  }

  function scheduleTypeset(panel) {
    if (typesetTimer) clearTimeout(typesetTimer);
    typesetTimer = setTimeout(function() {
      typesetHelpPanel(panel);
    }, 150);
  }

  function initObserver() {
    // The panel's id is namespaced by however app/main.R's own
    // module id is mounted (e.g. "app-help-help_panel"), which
    // is not fixed from this script's point of view — select by
    // the panel's class instead of guessing the exact id.
    var panel = document.querySelector('.help-offcanvas-resizable');
    if (!panel) return false;

    var observer = new MutationObserver(function() {
      if (isTypesetting) return;
      scheduleTypeset(panel);
    });
    observer.observe(panel, { childList: true, subtree: true });

    // Typeset once immediately in case content is already present
    scheduleTypeset(panel);
    return true;
  }

  function waitForPanel() {
    if (initObserver()) return;
    setTimeout(waitForPanel, 200);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', waitForPanel);
  } else {
    waitForPanel();
  }
})();
