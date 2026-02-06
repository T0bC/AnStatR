// Statistics Tab State Handler
// Manages the disabled/enabled state of the Statistics tab based on plotting selections

$(document).ready(function () {
    // Track current state
    let statisticsEnabled = false;

    // Handle state updates from Shiny
    Shiny.addCustomMessageHandler('statistics_tab_state', function (message) {
        statisticsEnabled = message.enabled;
        updateTabState();
    });

    function updateTabState() {
        // Find the Statistics tab link
        const statsTab = $('a.nav-link[data-value="statistics"]');

        if (statsTab.length === 0) return;

        if (statisticsEnabled) {
            // Enable the tab
            statsTab.removeClass('disabled-tab');
            statsTab.removeAttr('title');
            statsTab.css('pointer-events', '');
        } else {
            // Disable the tab visually
            statsTab.addClass('disabled-tab');
            statsTab.attr('title', 'Select measurement and X-axis columns in the Plotting tab first');
        }
    }

    // Intercept clicks on disabled Statistics tab
    $(document).on('click', 'a.nav-link[data-value="statistics"]', function (e) {
        if (!statisticsEnabled) {
            e.preventDefault();
            e.stopPropagation();

            // Show modal explaining what's needed
            showStatisticsRequirementsModal();
            return false;
        }
    });

    function showStatisticsRequirementsModal() {
        // Remove existing modal if present
        $('#statistics-requirements-modal').remove();

        const modalHtml = `
      <div class="modal fade" id="statistics-requirements-modal" tabindex="-1" aria-labelledby="statsReqModalLabel" aria-hidden="true">
        <div class="modal-dialog modal-dialog-centered">
          <div class="modal-content">
            <div class="modal-header bg-info text-white">
              <h5 class="modal-title" id="statsReqModalLabel">
                <i class="bi bi-info-circle me-2"></i>Statistics Tab Requirements
              </h5>
              <button type="button" class="btn-close btn-close-white" data-bs-dismiss="modal" aria-label="Close"></button>
            </div>
            <div class="modal-body">
              <p>To use the Statistics tab, you need to configure your data in the <strong>Plotting</strong> tab first:</p>
              <ol>
                <li><strong>Select measurement columns</strong> (Y-axis) - the variables you want to analyze</li>
                <li><strong>Select X-axis columns</strong> - the grouping variables for comparisons</li>
              </ol>
              <p class="text-muted small mb-0">
                The Statistics tab uses the same data selection, filtering, and processing settings from the Plotting tab.
              </p>
            </div>
            <div class="modal-footer">
              <button type="button" class="btn btn-secondary" data-bs-dismiss="modal">Close</button>
              <button type="button" class="btn btn-primary" id="go-to-plotting-btn">
                Go to Plotting Tab
              </button>
            </div>
          </div>
        </div>
      </div>
    `;

        $('body').append(modalHtml);

        const modal = new bootstrap.Modal(document.getElementById('statistics-requirements-modal'));
        modal.show();

        // Handle "Go to Plotting" button
        $('#go-to-plotting-btn').on('click', function () {
            modal.hide();
            // Navigate to Plotting tab
            $('a.nav-link[data-value="plotting"]').click();
        });

        // Clean up modal after hidden
        $('#statistics-requirements-modal').on('hidden.bs.modal', function () {
            $(this).remove();
        });
    }

    // Initial state check after Shiny connects
    $(document).on('shiny:connected', function () {
        // Default to disabled until we receive state from server
        updateTabState();
    });
});
