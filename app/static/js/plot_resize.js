// Plot resize handler for responsive plot outputs
// Reports container dimensions to Shiny for dynamic sizing
// Uses viewport-relative sizing for proper scaling on all monitor resolutions

// Debounce utility to limit the rate at which a function fires
function debounce(func, wait, immediate) {
    var timeout;
    return function () {
        var context = this, args = arguments;
        var later = function () {
            timeout = null;
            if (!immediate) func.apply(context, args);
        };
        var callNow = immediate && !timeout;
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
        if (callNow) func.apply(context, args);
    };
}

// Initialize container size reporting for a specific module
// targetId: the namespaced ID of the plots container
// windowInputId: the namespaced input ID to send size to
function initializeWindowSize(targetId, windowInputId) {
    var lastWidth = null;
    var lastHeight = null;

    var reportWindowSize = function () {
        if (window.Shiny && Shiny.setInputValue) {
            var currentWidth;
            var currentHeight;

            var viewportWidth = window.innerWidth;

            // Find the main content area width
            var sidebarLayout = document.querySelector('.bslib-sidebar-layout');
            var isCollapsed = sidebarLayout &&
                sidebarLayout.classList.contains('sidebar-collapsed');

            var mainContent = document.querySelector(
                '.bslib-sidebar-layout > :not(.sidebar):not(.collapse-toggle)'
            );

            if (mainContent && mainContent.offsetWidth > 0) {
                currentWidth = mainContent.offsetWidth - 32;
            } else {
                var sidebarWidth = 0;
                if (!isCollapsed) {
                    sidebarWidth = Math.min(
                        450, Math.max(320, viewportWidth * 0.33)
                    );
                }
                currentWidth = viewportWidth - sidebarWidth - 32;
            }

            // Height: measure responsive-plot container or calculate from card
            var responsivePlot = document.querySelector('.responsive-plot');
            var plotCardBody = document.querySelector('.plot-card-body');

            if (responsivePlot && responsivePlot.offsetHeight > 0) {
                currentHeight = responsivePlot.offsetHeight;
            } else if (plotCardBody && plotCardBody.offsetHeight > 0) {
                currentHeight = plotCardBody.offsetHeight - 16;
            } else {
                var navbar = document.querySelector('.navbar');
                var navbarHeight = navbar ? navbar.offsetHeight : 56;
                var cardHeight = (window.innerHeight - navbarHeight) * 0.50;
                currentHeight = Math.round(cardHeight - 56);
            }

            // Ensure minimum reasonable dimensions
            currentWidth = Math.max(400, currentWidth);
            currentHeight = Math.max(250, currentHeight);

            // Only send if values actually changed
            if (
                Math.abs(currentWidth - lastWidth) > 5 ||
                Math.abs(currentHeight - lastHeight) > 5
            ) {
                lastWidth = currentWidth;
                lastHeight = currentHeight;

                Shiny.setInputValue(windowInputId, {
                    width: currentWidth,
                    height: currentHeight
                }, { priority: 'event' });
            }
        }
    };

    var debouncedReportSize = debounce(reportWindowSize, 250);

    $(window).on('resize', debouncedReportSize);

    // Update when sidebar is toggled
    $(document).on('click', '.collapse-toggle', function () {
        setTimeout(reportWindowSize, 400);
    });

    // Watch for sidebar class changes via MutationObserver
    var sidebarObserver = new MutationObserver(function (mutations) {
        mutations.forEach(function (mutation) {
            if (mutation.attributeName === 'class') {
                setTimeout(reportWindowSize, 400);
            }
        });
    });

    // Report on tab switches
    $(document).on('shown.bs.tab', function () {
        setTimeout(reportWindowSize, 50);
    });

    // Initial report after page load
    $(document).on('shiny:connected', function () {
        setTimeout(reportWindowSize, 100);

        var sidebarLayout = document.querySelector('.bslib-sidebar-layout');
        if (sidebarLayout) {
            sidebarObserver.observe(
                sidebarLayout,
                { attributes: true, attributeFilter: ['class'] }
            );
        }
    });

    // Re-report when container content changes (uiOutput renders).
    //
    // The layout read below (offsetWidth, plus the querySelector/offsetWidth
    // reads inside reportWindowSize) must never run synchronously inside the
    // MutationObserver callback: that callback fires from within Shiny's
    // WebSocket message handler while the new DOM is being inserted, so the
    // read forces a synchronous full-document layout mid-insertion. On the
    // deployed app a single one of those was measured at 5,608 ms.
    //
    // Instead the measurement is deferred to idle time (or the next frame),
    // by which point the browser has laid out the new content on its own
    // schedule and the read is free. It is also coalesced, so one burst of
    // mutations produces one measurement rather than one per batch.
    var sizeReportPending = false;

    var deferIdle = function (fn) {
        if (window.requestIdleCallback) {
            window.requestIdleCallback(fn, { timeout: 500 });
        } else {
            setTimeout(fn, 50);
        }
    };

    var scheduleSizeReport = function () {
        if (sizeReportPending) return;
        sizeReportPending = true;
        deferIdle(function () {
            sizeReportPending = false;
            var container = document.getElementById(targetId);
            if (container && container.offsetWidth > 0) {
                reportWindowSize();
            }
        });
    };

    var contentObserver = new MutationObserver(scheduleSizeReport);

    // Scope the observer to this module's own container instead of the whole
    // document. Previously every module that called initializeWindowSize()
    // observed document.body with subtree:true, so a DOM insertion in any one
    // tab woke all of them.
    // targetId is a uiOutput container, so Shiny replaces its children and
    // the element itself persists -- safe to observe once and keep.
    var attached = false;
    var attempts = 0;

    var observeContainer = function () {
        if (attached) return;
        var container = document.getElementById(targetId);
        if (!container) {
            // initializeWindowSize() is itself called from a shiny:connected
            // handler, so there is no later lifecycle event to wait on.
            // Retry briefly in case the container has not been inserted yet.
            if (++attempts < 20) setTimeout(observeContainer, 100);
            return;
        }
        attached = true;
        contentObserver.observe(
            container,
            { childList: true, subtree: true }
        );
    };

    observeContainer();
}

// =============================================================================
// DEBUG: Visible on-screen overlay for environments without a console.
// Set ANSTATR_DEBUG = true to enable. Disabled by default.
// =============================================================================

var ANSTATR_DEBUG = false;

(function () {
    if (!ANSTATR_DEBUG) {
        window._anstatrDbg = function () { };
        return;
    }

    var debugLines = [];
    var debugEl = null;

    function ensureOverlay() {
        if (debugEl) return;
        debugEl = document.createElement('div');
        debugEl.id = 'anstatr-debug-overlay';
        debugEl.style.cssText = [
            'position:fixed', 'bottom:0', 'left:0', 'right:0',
            'max-height:40vh', 'overflow:auto', 'background:rgba(0,0,0,0.85)',
            'color:#0f0', 'font:11px/1.4 monospace', 'padding:8px 12px',
            'z-index:99999', 'pointer-events:auto', 'white-space:pre-wrap'
        ].join(';');
        document.body.appendChild(debugEl);
    }

    function dbg(msg) {
        debugLines.push('[' + new Date().toLocaleTimeString() + '] ' + msg);
        if (debugLines.length > 80) debugLines.shift();
        ensureOverlay();
        debugEl.textContent = debugLines.join('\n');
        debugEl.scrollTop = debugEl.scrollHeight;
    }

    window._anstatrDbg = dbg;
})();

// =============================================================================
// Post-render resize hook for ggiraph SVGs
// Ensures SVGs fill their container in all environments (browsers + IDE preview).
// With rescale = FALSE, ggiraph sets width/height as SVG attributes.
// This observer removes those attributes after render so CSS can control sizing.
// =============================================================================

(function () {
    function fixGirafeSvg(svg) {
        var container = svg.closest('.responsive-plot');
        if (!container) return;

        // Remove SVG width/height attributes so CSS can control sizing
        if (svg.hasAttribute('width') && svg.hasAttribute('viewBox')) {
            svg.removeAttribute('width');
            svg.removeAttribute('height');
        }

        svg.style.setProperty('width', '100%', 'important');
        svg.style.setProperty('height', 'auto', 'important');
        svg.style.display = 'block';

        var girafeContainer = svg.closest('.girafe_container_std');
        if (girafeContainer) {
            girafeContainer.style.width = '100%';
        }

        var widget = svg.closest('.html-widget');
        if (widget) {
            widget.style.setProperty('width', '100%', 'important');
        }
    }

    function fixAllGirafeSvgs() {
        var svgs = document.querySelectorAll(
            '.responsive-plot .girafe_container_std svg'
        );
        for (var i = 0; i < svgs.length; i++) {
            fixGirafeSvg(svgs[i]);
        }
    }

    // Coalesce all mutations in a frame into a single sweep. The previous
    // version ran querySelectorAll() on every added node, so inserting a
    // large plot meant walking its whole subtree once per node. One
    // document-level sweep on the next frame does the same work far cheaper,
    // and keeps the style writes out of the WebSocket message handler.
    var fixScheduled = false;

    function scheduleFix() {
        if (fixScheduled) return;
        fixScheduled = true;
        requestAnimationFrame(function () {
            fixScheduled = false;
            fixAllGirafeSvgs();
        });
    }

    var observer = new MutationObserver(function (mutations) {
        for (var i = 0; i < mutations.length; i++) {
            if (mutations[i].addedNodes.length > 0) {
                scheduleFix();
                return;
            }
        }
    });

    document.addEventListener('DOMContentLoaded', function () {
        // This block exists verbatim in app/js/index.js too, and both files
        // are loaded: main.R adds this file explicitly, while rhino's
        // with_head_tags() auto-injects app.min.js (built from index.js).
        // Without this claim the observer would attach twice and every DOM
        // insertion would be swept twice. First one in wins; the other
        // becomes a no-op.
        if (window.__anstatrGirafeObserverAttached) return;
        window.__anstatrGirafeObserverAttached = true;

        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
        fixAllGirafeSvgs();
    });
})();