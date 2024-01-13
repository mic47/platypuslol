function onKeyPress(event) {
  var cls = 'onpress' + event.key;
  var elements = Array
    .from(document.getElementsByClassName(cls))
    .filter((tag) => tag.tagName.toLowerCase() == 'a');
  if (elements.length == 0) {
    return;
  }
  for (var i = 1; i < elements.length; i++) {
    var href = elements[i].href;
    if (href != undefined && href != null) {
      window.open(href, '_blank');
    }
  }
  var href = elements[0].href;
  if (href != undefined && href != null) {
    window.location.href = href;
  }
}

function onLoad() {
  var input = document
    .querySelector('body')
    .addEventListener('keydown', onKeyPress);
}
