// =============================================================================
// Disabled Tab State Handler
// Manages grayed-out / locked tabs based on prerequisite conditions
// =============================================================================

$(document).ready(function () {
    // Track disabled state per tab value
    var disabledTabs = {};

    // Handle state updates from Shiny
    // message: { tab: "summary", enabled: true/false, reason: "..." }
    Shiny.addCustomMessageHandler('tab_disabled_state', function (message) {
        disabledTabs[message.tab] = {
            enabled: message.enabled,
            reason: message.reason || 'Prerequisites not met.'
        };
        updateDisabledTab(message.tab);
    });

    function updateDisabledTab(tabValue) {
        var tabLink = $('a.nav-link[data-value="' + tabValue + '"]');
        if (tabLink.length === 0) return;

        var state = disabledTabs[tabValue];
        if (!state) return;

        if (state.enabled) {
            tabLink.removeClass('disabled-tab');
            tabLink.removeAttr('title');
        } else {
            tabLink.addClass('disabled-tab');
            tabLink.attr('title', state.reason);
        }
    }

    // Intercept clicks on disabled tabs
    $(document).on('click', 'a.nav-link.disabled-tab', function (e) {
        e.preventDefault();
        e.stopPropagation();

        var tabValue = $(this).data('value');
        var state = disabledTabs[tabValue];
        var reason = state ? state.reason : 'Prerequisites not met.';

        showRequirementsModal(tabValue, reason);
        return false;
    });

    function showRequirementsModal(tabValue, reason) {
        var modalId = 'disabled-tab-modal';
        $('#' + modalId).remove();

        var modalHtml =
            '<div class="modal fade" id="' + modalId + '" tabindex="-1" aria-hidden="true">' +
            '  <div class="modal-dialog modal-dialog-centered">' +
            '    <div class="modal-content">' +
            '      <div class="modal-header bg-info text-white">' +
            '        <h5 class="modal-title">' +
            '          <i class="bi bi-info-circle me-2"></i>Tab Locked' +
            '        </h5>' +
            '        <button type="button" class="btn-close btn-close-white" data-bs-dismiss="modal" aria-label="Close"></button>' +
            '      </div>' +
            '      <div class="modal-body">' +
            '        <p>' + reason + '</p>' +
            '      </div>' +
            '      <div class="modal-footer">' +
            '        <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>' +
            '      </div>' +
            '    </div>' +
            '  </div>' +
            '</div>';

        $('body').append(modalHtml);
        var modal = new bootstrap.Modal(document.getElementById(modalId));
        modal.show();

        $('#' + modalId).on('hidden.bs.modal', function () {
            $(this).remove();
        });
    }

    // Re-apply states after Shiny reconnects
    $(document).on('shiny:connected', function () {
        Object.keys(disabledTabs).forEach(function (tab) {
            updateDisabledTab(tab);
        });
    });
});

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