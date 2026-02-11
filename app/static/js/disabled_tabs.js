// =============================================================================
// Disabled Tab State Handler
// Manages grayed-out / locked tabs based on prerequisite conditions
// =============================================================================

// Track disabled state per tab value
var disabledTabs = {};

function updateDisabledTab(tabValue) {
    var tabLink = $('a.nav-link[data-value="' + tabValue + '"]');
    if (tabLink.length === 0) return;

    var state = disabledTabs[tabValue];
    if (!state) return;

    if (state.enabled) {
        tabLink.removeClass('disabled-tab');
        tabLink.removeAttr('title');
        tabLink.css('pointer-events', '');
    } else {
        tabLink.addClass('disabled-tab');
        tabLink.attr('title', state.reason);
    }
}

function showRequirementsModal(reason) {
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

// Intercept clicks on disabled tabs (delegated, works at any time)
$(document).on('click', 'a.nav-link.disabled-tab', function (e) {
    e.preventDefault();
    e.stopPropagation();

    var tabValue = $(this).data('value');
    var state = disabledTabs[tabValue];
    var reason = state ? state.reason : 'Prerequisites not met.';

    showRequirementsModal(reason);
    return false;
});

// Register Shiny custom message handler
// Uses shiny:sessioninitialized which fires after Shiny is ready but before
// the server sends its first batch of messages
$(document).on('shiny:sessioninitialized', function () {
    Shiny.addCustomMessageHandler('tab_disabled_state', function (message) {
        disabledTabs[message.tab] = {
            enabled: message.enabled,
            reason: message.reason || 'Prerequisites not met.'
        };
        updateDisabledTab(message.tab);
    });
});

// Re-apply states after Shiny reconnects
$(document).on('shiny:connected', function () {
    Object.keys(disabledTabs).forEach(function (tab) {
        updateDisabledTab(tab);
    });
});
