<%#= escape_javascript(etc) %>

//$( document ).ready(function() {
//  $( ".navbar .container-fluid" ).append( '<a href="https://github.com/PlanktonTeam/BioOceanObserver" target="_blank"><img src="www/github-mark/github-mark-white.png" style="height:40px; margin-right//:0px;" align="right"></a>' );
//});

// Force Plotly plots to resize when window is resized
$(window).on('resize', function() {
  // Find all plotly plot elements and resize them
  $('.plotly').each(function() {
    var gd = this;
    if (gd && typeof Plotly !== 'undefined') {
      Plotly.Plots.resize(gd);
    }
  });
});

// Also handle resize when Shiny recalculates outputs
$(document).on('shiny:value', function(event) {
  if (event.target.classList && event.target.classList.contains('plotly')) {
    setTimeout(function() {
      if (typeof Plotly !== 'undefined') {
        Plotly.Plots.resize(event.target);
      }
    }, 100);
  }
});
scape_javascript(etc) %>

//$( document ).ready(function() {
//  $( ".navbar .container-fluid" ).append( '<a href="https://github.com/PlanktonTeam/BioOceanObserver" target=”_blank”><img src="www/github-mark/github-mark-white.png" style="height:40px; margin-right//:0px;" align="right"></a>' );
//});
