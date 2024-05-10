// https://stackoverflow.com/questions/3972014/get-contenteditable-caret-position
function getSelectionOffsetWithin(element) {
  var doc = element.ownerDocument || element.document;
  var win = doc.defaultView || doc.parentWindow;
  var sel;
  var offsets = {start: 0, end: 0};

  if (typeof win.getSelection != "undefined") {
      sel = win.getSelection();
      if (sel.rangeCount > 0) {
          var range = sel.getRangeAt(0);

          // Calculate start offset
          offsets.start = calculateOffset(element, range.startContainer, range.startOffset);

          // Calculate end offset
          offsets.end = calculateOffset(element, range.endContainer, range.endOffset);
      }
  }

  return offsets;
}

function calculateOffset(root, container, offset) {
  var children = Array.from(root.childNodes);
  var len = 0;
  var node;

  for (node of children) {
      if (node.nodeType === Node.ELEMENT_NODE) {
          if (node.contains(container)) {
              if (node === container) {
                  len += offset;
              } else {
                  len += calculateOffset(node, container, offset);
              }
              break;
          } else {
              len += node.innerText.replace(/\n+/g, ' ').length;
          }
      } else if (node.nodeType === Node.TEXT_NODE && node === container) {
          len += offset;
          break;
      }
  }

  return len;
}

$( document ).ready(function() {

  document.addEventListener('mouseup', function () {

  var el = document.getElementById("article");
  var offsets = getSelectionOffsetWithin(el);
  const tagPositionValue = offsets.start + '-' + offsets.end;
  Shiny.setInputValue('document_code_ui_1-tag_position', tagPositionValue);
  console.log(tagPositionValue)
})
})

  

Shiny.addCustomMessageHandler('getIframeContent', function(message) {
  var iframe = document.getElementsByTagName('iframe')[0];
  var res = iframe.contentDocument.getElementById('quickCodeInput');
  var quickodeValue = res.dataset.quickode;
  Shiny.setInputValue('document_code_ui_1-quickcode', quickodeValue);
});

Shiny.addCustomMessageHandler('refreshIframe', function(message) {
  var iframe = document.getElementsByTagName('iframe')[0];
  iframe.src = iframe.src;
});
