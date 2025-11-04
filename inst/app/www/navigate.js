Shiny.addCustomMessageHandler("navigate", function(tabPath) {
  if (!Array.isArray(tabPath) || tabPath.length !== 2) return;

  var navbarTab = tabPath[0];
  var nestedTab = tabPath[1];

  // Step 1: Click navbar tab
  var navbarEl = document.querySelector('[data-value="' + navbarTab + '"]');
  if (navbarEl) {
    navbarEl.click();

    // Step 2: Wait for nested tab to appear
    var attempts = 0;
    var maxAttempts = 20;
    var interval = setInterval(function() {
      var nestedEl = document.querySelector('[data-value="' + nestedTab + '"]');
      if (nestedEl && nestedEl.offsetParent !== null) { // visible check
        nestedEl.click();
        clearInterval(interval);
      }
      attempts++;
      if (attempts >= maxAttempts) {
        clearInterval(interval);
        console.warn("Nested tab not found or not visible: " + nestedTab);
      }
    }, 200);
  } else {
    console.warn("Navbar tab not found: " + navbarTab);
  }
});
