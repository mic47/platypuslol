function elementByXpath(path) {
  let data = document.evaluate('//body/ul/li/a', document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
  var output = [];
  for (var i = 0; i < data.snapshotLength; i++) {
    output.push(data.snapshotItem(i));
  }
  return output
}

function reset(event) {
  var query = document.getElementById('query');
  query.textContent = "";
  setVisibility("");
}

function setVisibility(pressed) {
  let pre = 'onpress' + pressed;
  let par = 'onparent' + pressed
  let allElements = Array.from(document.getElementsByClassName('toogable'));
  allElements
    .map((el) => {
      let names = el.classList;
      var onpress = undefined;
      var onparent = undefined;
      for (var i = 0; i < names.length; i++) {
        let name = names[i];
        if (name.startsWith('onpress')) {
          onpress = name;
        }
        if (name.startsWith('onparent')) {
          onparent = name;
        }
      }
      if (onpress == undefined || onparent == undefined) {
        return;
      }
      if (!onpress.startsWith(pre) && !pre.startsWith(onpress)) {
        el.parentElement.style.display = 'none';
        return
      }
      if (!par.startsWith(onparent)) {
        el.parentElement.style.display = 'none';
        return
      }
      el.parentElement.style.display = '';
    })
}

function onKeyPress(event) {
  var query = document.getElementById('query');
  var pressed = (query.textContent ?? "").trim();
  if (event.key.length == 1) {
    pressed += event.key;
    query.textContent = pressed;
  } else if (event.key == 'Backspace') {
    pressed = pressed.slice(0, -1);
    query.textContent= pressed;
    setVisibility(pressed);
    return;
  } else if (event.key == 'Escape') {
    pressed = "";
    query.textContent = pressed;
    setVisibility(pressed);
    return;
  }
  var cls = 'onpress' + pressed;

  var elements = Array
    .from(document.getElementsByClassName(cls))
    .filter((tag) => tag.tagName.toLowerCase() == 'a');
  if (elements.length == 0) {
    setVisibility(pressed);
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
  window.addEventListener('pageshow', reset);
  window.addEventListener('pagehide', reset);
  window.addEventListener('pagehide', reset);
  window.addEventListener('beforeunload', reset);
  var query = document.getElementById('query');
  var pressed = (query.textContent ?? "").trim();
  setVisibility(pressed);
}
