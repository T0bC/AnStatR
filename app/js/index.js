// =============================================================================
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

    // Re-report when container content changes (uiOutput renders)
    var contentObserver = new MutationObserver(function () {
        var container = document.getElementById(targetId);
        if (container && container.offsetWidth > 0) {
            setTimeout(reportWindowSize, 50);
        }
    });

    $(document).ready(function () {
        contentObserver.observe(
            document.body,
            { childList: true, subtree: true }
        );
    });
}

// =============================================================================
// DEBUG: Visible on-screen overlay for environments without a console.
// Set TEXAN_DEBUG = true to enable. Disabled by default.
// =============================================================================

var TEXAN_DEBUG = false;

(function () {
    if (!TEXAN_DEBUG) {
        window._texanDbg = function () { };
        return;
    }

    var debugLines = [];
    var debugEl = null;

    function ensureOverlay() {
        if (debugEl) return;
        debugEl = document.createElement('div');
        debugEl.id = 'texan-debug-overlay';
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

    window._texanDbg = dbg;
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

    var observer = new MutationObserver(function (mutations) {
        for (var i = 0; i < mutations.length; i++) {
            var added = mutations[i].addedNodes;
            for (var j = 0; j < added.length; j++) {
                var node = added[j];
                if (node.nodeType !== 1) continue;
                if (node.tagName === 'svg') {
                    fixGirafeSvg(node);
                } else if (node.querySelectorAll) {
                    var svgs = node.querySelectorAll(
                        '.girafe_container_std svg'
                    );
                    for (var k = 0; k < svgs.length; k++) {
                        fixGirafeSvg(svgs[k]);
                    }
                }
            }
        }
    });

    document.addEventListener('DOMContentLoaded', function () {
        observer.observe(document.body, {
            childList: true,
            subtree: true
        });
        fixAllGirafeSvgs();
    });
})();

// =============================================================================
// Keep selectize dropdowns open for multi-select inputs
// Patches Selectize.prototype.close so that multi-select dropdowns
// remain open while the control is focused.
// =============================================================================

(function () {
    function applyPrototypePatch() {
        var Ctor = window.Selectize;
        if (!Ctor || !Ctor.prototype) return false;
        if (Ctor.prototype._close_patched) return true;

        var origAddItem = Ctor.prototype.addItem;
        Ctor.prototype.addItem = function (value, silent) {
            var isMulti = this.settings.maxItems !== 1;
            var result = origAddItem.apply(this, arguments);
            if (isMulti) {
                var self = this;
                setTimeout(function () {
                    self.open();
                    self.$control_input[0].focus();
                }, 0);
            }
            return result;
        };

        Ctor.prototype._close_patched = true;
        return true;
    }

    // Poll until Selectize is available (loaded async by Shiny)
    var attempts = 0;
    var poller = setInterval(function () {
        attempts++;
        if (applyPrototypePatch() || attempts > 100) {
            clearInterval(poller);
        }
    }, 100);
})();