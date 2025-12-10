// Plot resize handler for ggiraph outputs
// Reports container dimensions to Shiny for dynamic SVG sizing
// Uses viewport-relative sizing for proper scaling on all monitor resolutions

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

// Initialize container size reporting for a specific module
// targetId: the namespaced ID of the plots container (e.g., "plotting-plots")
// windowInputId: the namespaced input ID to send size to
function initializeWindowSize(targetId, windowInputId) {

    // Track last reported size to avoid duplicate updates
    var lastWidth = null;
    var lastHeight = null;

    // Function to report container size to Shiny (only if changed)
    // Width: measured from main content area
    // Height: measured from actual plot card body (CSS controls card height)
    var reportWindowSize = function () {
        if (window.Shiny && Shiny.setInputValue) {
            var currentWidth;
            var currentHeight;

            // Get viewport dimensions for fallback calculations
            var viewportWidth = window.innerWidth;

            // Find the main content area width
            var sidebarLayout = document.querySelector('.bslib-sidebar-layout');
            var isCollapsed = sidebarLayout && sidebarLayout.classList.contains('sidebar-collapsed');

            // Try to measure main content directly
            var mainContent = document.querySelector('.bslib-sidebar-layout > :not(.sidebar):not(.collapse-toggle)');

            if (mainContent && mainContent.offsetWidth > 0) {
                // Use actual main content width minus padding
                currentWidth = mainContent.offsetWidth - 32;
            } else {
                // Fallback: calculate from viewport minus sidebar
                // Sidebar width uses clamp(320px, 33vw, 450px) when open
                var sidebarWidth = 0;
                if (!isCollapsed) {
                    sidebarWidth = Math.min(450, Math.max(320, viewportWidth * 0.33));
                }
                currentWidth = viewportWidth - sidebarWidth - 32;
            }

            // Height: measure responsive-plot container or calculate from card height
            // Card height is set by CSS: calc((100vh - 56px) * 0.50)
            // Card body has ~8px padding, card header is ~40px
            var responsivePlot = document.querySelector('.responsive-plot');
            var plotCardBody = document.querySelector('.plot-card-body');

            if (responsivePlot && responsivePlot.offsetHeight > 0) {
                // Best: measure the actual responsive-plot container
                currentHeight = responsivePlot.offsetHeight;
            } else if (plotCardBody && plotCardBody.offsetHeight > 0) {
                // Fallback: measure card body minus padding
                currentHeight = plotCardBody.offsetHeight - 16;
            } else {
                // Last resort: calculate from viewport using same formula as CSS
                var navbar = document.querySelector('.navbar');
                var navbarHeight = navbar ? navbar.offsetHeight : 56;
                var cardHeight = (window.innerHeight - navbarHeight) * 0.50;
                // Subtract card header (~40px) and padding (~16px)
                currentHeight = Math.round(cardHeight - 56);
            }

            // Ensure minimum reasonable dimensions
            currentWidth = Math.max(400, currentWidth);
            currentHeight = Math.max(250, currentHeight);

            // Only send if values actually changed (with small tolerance for rounding)
            if (Math.abs(currentWidth - lastWidth) > 5 || Math.abs(currentHeight - lastHeight) > 5) {
                lastWidth = currentWidth;
                lastHeight = currentHeight;

                Shiny.setInputValue(windowInputId, {
                    width: currentWidth,
                    height: currentHeight
                }, { priority: 'event' });
            }
        }
    };

    // Debounced version (250ms delay for resize dragging)
    var debouncedReportSize = debounce(reportWindowSize, 250);

    // Update on window resize
    $(window).on('resize', debouncedReportSize);

    // Update when sidebar is toggled (bslib sidebar collapse)
    // Use event delegation and watch for class changes on the layout
    $(document).on('click', '.collapse-toggle', function () {
        // Delay to let the sidebar animation complete
        setTimeout(reportWindowSize, 400);
    });

    // Also watch for sidebar state changes via MutationObserver
    var sidebarObserver = new MutationObserver(function (mutations) {
        mutations.forEach(function (mutation) {
            if (mutation.attributeName === 'class') {
                // Sidebar collapsed/expanded - wait for animation then report
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

        // Start observing sidebar layout for class changes (collapse state)
        var sidebarLayout = document.querySelector('.bslib-sidebar-layout');
        if (sidebarLayout) {
            sidebarObserver.observe(sidebarLayout, { attributes: true, attributeFilter: ['class'] });
        }
    });

    // Re-report when container content changes (uiOutput renders)
    var contentObserver = new MutationObserver(function (mutations) {
        var container = document.getElementById(targetId);
        if (container && container.offsetWidth > 0) {
            setTimeout(reportWindowSize, 50);
        }
    });

    // Start observing once document is ready
    $(document).ready(function () {
        contentObserver.observe(document.body, { childList: true, subtree: true });
    });
}
