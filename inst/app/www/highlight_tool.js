$(document).ready(function() {
  Shiny.addCustomMessageHandler('highlightSegments', function(message) {
    const processSegments = (elements, styleType) => {
      elements.forEach(segment_coded => {
        const colors = segment_coded.getAttribute('data-color').split(' | ');
        const color = averageColor(colors);
        console.log('Style Type:', styleType);
        console.log('Colors:', colors);
        console.log('Computed Color:', color);

        if (styleType === 'underline') {
          segment_coded.style.textDecoration = `underline ${color}`;
          segment_coded.style.backgroundColor = ''; // Clear background if previously set
        } else {
          segment_coded.style.backgroundColor = color;
          segment_coded.style.textDecoration = ''; // Clear underline if previously set
        }
      });
    };

    const ids = Array.isArray(message.ids) ? message.ids : [message.ids]; // Ensure ids is an array

    const observer = new MutationObserver((mutationsList, observer) => {
      ids.forEach(id => {
        const container = document.getElementById(id);
        if (container) {
          const segments = container.querySelectorAll('.segment-coded');
          if (segments.length > 0) {
            console.log(`Segments found in #${id}:`, segments.length);
            processSegments(segments, message.styleType || 'background');
            observer.disconnect(); // Stop observing once the elements are processed
          }
        } else {
          console.log(`Container with ID #${id} not found.`);
        }
      });
    });

    // Start observing the document body for changes
    observer.observe(document.body, { childList: true, subtree: true });
  });
});

function averageColor(colors) {
  const rgbColors = colors.map(color => {
    const matches = color.match(/\d+/g);
    return matches ? matches.map(Number) : [0, 0, 0];
  });

  const avgColor = rgbColors.reduce((acc, color) => {
    return acc.map((value, index) => value + color[index]);
  }, [0, 0, 0]).map(value => Math.round(value / colors.length));

  return `rgb(${avgColor.join(',')})`;
}


function averageColor(colors) {
  const rgbColors = colors.map(color => {
      const matches = color.match(/\d+/g);
      console.log(matches);
      return matches ? matches.map(Number) : [0, 0, 0];
  });

  const avgColor = rgbColors.reduce((acc, color) => {
      return acc.map((value, index) => value + color[index]);
  }, [0, 0, 0]).map(value => Math.round(value / colors.length));

  return `rgb(${avgColor.join(',')})`;
}
