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
// targetId: the namespaced ID of the plots container (for visibility check)
// windowInputId: the namespaced input ID to send window size to
function initializeWindowSize(targetId, windowInputId) {
    var targetSelector = '#' + targetId;

    // Function to report window size to Shiny
    var reportWindowSize = function () {
        // DEBUG: Log to browser console
        console.log('reportWindowSize called, Shiny available:', !!window.Shiny, 'windowInputId:', windowInputId);

        if (window.Shiny && Shiny.setInputValue) {
            // Always report - let R handle visibility logic if needed
            Shiny.setInputValue(windowInputId, {
                width: window.innerWidth,
                height: window.innerHeight
            }, { priority: "event" });
            console.log('Sent to Shiny:', windowInputId, window.innerWidth, window.innerHeight);
        }
    };

    // Debounced version (100ms delay)
    var debouncedReportWindowSize = debounce(reportWindowSize, 100);

    // Update on window resize
    $(window).on('resize', debouncedReportWindowSize);

    // Report when hidden outputs become visible (tab switches)
    $(document).on('shiny:visualchange', debouncedReportWindowSize);

    // Report when Bootstrap tabs are shown (for bslib nav panels)
    $(document).on('shown.bs.tab', debouncedReportWindowSize);

    // Initial report
    debouncedReportWindowSize();
}
