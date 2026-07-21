$( document ).ready(function() {
  Shiny.addCustomMessageHandler('fun', function(arg) {
  
  });

  // bslib::navset_pill nested inside nav_menu() does not reliably fire its
  // Shiny input binding when the parent nav_menu has never been visited.
  //
  // Root cause: Bootstrap 5 fires shown.bs.tab on the newly-activated <a>
  // element; Shiny's BootstrapTabInputBinding listens for this event on the
  // <ul class="shiny-tab-input"> element itself. When the navset_pill is
  // inside a nav_menu dropdown that has never been opened, the <ul> is
  // hidden and Shiny may not have subscribed to it yet, so the event never
  // reaches the binding and input$NRSspatp stays NULL on all tabs.
  //
  // Fix: listen at the document level, find the <ul> ancestor, and push the
  // value directly via Shiny.setInputValue() with event priority.
  //
  // DOM structure produced by bslib::navset_pill:
  //   <ul class="nav nav-pills shiny-tab-input" id="<ns>-<tabsetPanel_id>">
  //     <li><a data-value="1" ...>Tab 1</a></li>
  //     <li><a data-value="2" ...>Tab 2</a></li>
  //   </ul>
  $(document).on('shown.bs.tab', function(e) {
    // e.target is the newly-activated <a> (or occasionally <li> in some BS versions)
    var $target = $(e.target);

    // Resolve the <a> element that carries data-value
    var $anchor = $target.is('a') ? $target : $target.find('a[data-value]').first();
    if ($anchor.length === 0) $anchor = $target.closest('a[data-value]');

    var tabValue = $anchor.attr('data-value');
    if (tabValue === undefined) return;

    // Walk up to the nearest <ul class="shiny-tab-input"> to get the input id
    var $ul = $anchor.closest('ul.shiny-tab-input');
    if ($ul.length === 0) return;

    var inputId = $ul.attr('id');
    if (!inputId) return;

    // Push the value to Shiny — priority:'event' ensures renderUI re-runs
    // even if the value hasn't changed (e.g. clicking the already-active tab)
    Shiny.setInputValue(inputId, tabValue, {priority: 'event'});
  });
});
