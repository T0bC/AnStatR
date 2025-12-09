// Plot resize handler for ggiraph outputs
// Reports window size to Shiny for dynamic SVG sizing

// Debounce function to limit the rate at which a function can fire
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

// Initialize window size reporting for a specific module
// targetId: unused, kept for API compatibility
// windowInputId: the namespaced input ID to send window size to
function initializeWindowSize(targetId, windowInputId) {

    // Track last reported size to avoid duplicate updates
    var lastWidth = null;
    var lastHeight = null;

    // Function to report window size to Shiny (only if changed)
    var reportWindowSize = function () {
        if (window.Shiny && Shiny.setInputValue) {
            var currentWidth = window.innerWidth;
            var currentHeight = window.innerHeight;

            // Only send if values actually changed
            if (currentWidth !== lastWidth || currentHeight !== lastHeight) {
                lastWidth = currentWidth;
                lastHeight = currentHeight;

                Shiny.setInputValue(windowInputId, {
                    width: currentWidth,
                    height: currentHeight
                });  // No priority: "event" - let Shiny deduplicate
            }
        }
    };

    // Debounced version (250ms delay for resize dragging)
    var debouncedReportWindowSize = debounce(reportWindowSize, 250);

    // Update on window resize only
    $(window).on('resize', debouncedReportWindowSize);

    // Report on tab switches (but not on every visual change)
    // shown.bs.tab fires when Bootstrap tabs become visible
    $(document).on('shown.bs.tab', function () {
        // Small delay to let layout settle
        setTimeout(reportWindowSize, 50);
    });

    // Initial report after page load
    $(document).on('shiny:connected', function () {
        setTimeout(reportWindowSize, 100);
    });
}
