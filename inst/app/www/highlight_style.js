$(document).ready(function() {
  // Listen for messages of type 'toggleStyle'
  Shiny.addCustomMessageHandler('toggleStyle', function(mode) {
    // Loop over all elements with the class 'segment'
    $('.segment').each(function() {
      var highlightTag = $(this);
      var currentBackgroundColor = highlightTag.css('background-color');
      var currentTextDecorationColor = highlightTag.css('text-decoration-color');

      // Determine the new mode and apply styles accordingly
      if (mode === 'underline') {
        // If switching to underline, use the current background color for the underline
        highlightTag.css('background-color', 'transparent'); // Clear background color
        highlightTag.css('text-decoration', 'underline'); // Apply underline
        highlightTag.css('text-decoration-color', currentBackgroundColor); // Use the current background color for underline
      } else if (mode === 'background') {
        // If switching to background, use the current underline color for the background
        highlightTag.css('background-color', currentTextDecorationColor); // Apply underline color as background color
        highlightTag.css('text-decoration', ''); // Remove underline
        highlightTag.css('text-decoration-color', ''); // Remove underline color
      }
    });
  });
});
