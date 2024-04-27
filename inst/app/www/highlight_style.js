$( document ).ready(function(event) {
    // Listen for messages of type 'toggleStyle'
    Shiny.addCustomMessageHandler('toggleStyle', function(mode) {
      // Loop over all elements with the class 'segment'
      $('.segment').each(function() {
        var higlight_tag = $(this);
        var original_background_color = higlight_tag.data('color'); // Get the original color from the data-color attribute
        // Determine the new mode and apply styles accordingly
        if (mode === 'underline') {
          higlight_tag.css('background-color', 'transparent'); // Clear background color
          higlight_tag.css('text-decoration', 'underline'); // Apply underline
          higlight_tag.css('text-decoration-color', original_background_color); // Use the original color for underline
        } else if (mode === 'background') {
          higlight_tag.css('background-color', original_background_color); // Apply original color as background color
          higlight_tag.css('text-decoration', ''); // Remove underline
          higlight_tag.css('text-decoration-color', ''); // Remove underline color
        }
      });
    });
});