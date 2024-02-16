function elementByXpath(path) {
  let data = document.evaluate('//body/ul/li/a', document, null, XPathResult.ORDERED_NODE_SNAPSHOT_TYPE, null);
  var output = [];
  for (var i = 0; i < data.snapshotLength; i++) {
    output.push(data.snapshotItem(i));
  }
  return output
}

function reset(event) {
  setQuery("");
}

function setQuery(queryString) {
  var query = document.getElementById('query');
  query.value= queryString;
  setVisibility(queryString);
}

function setVisibility(pressed) {
  var expand = false;
  if (pressed.indexOf('e') > -1) {
    pressed = pressed.replace('e', '');
    expand = true;
  }
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
      if (!par.startsWith(onparent) && !expand) {
        el.parentElement.style.display = 'none';
        return
      }
      el.parentElement.style.display = '';
    })
}

function onKeyPress(event) {
  var query = document.getElementById('query');
  var pressed = (query.value ?? "").trim();
  if (event.key.length == 1) {
    pressed += event.key;
    query.value = pressed;
  } else if (event.key == 'Backspace') {
    pressed = pressed.slice(0, -1);
    query.value = pressed;
    setVisibility(pressed);
    return;
  } else if (event.key == 'Escape') {
    pressed = "";
    query.value = pressed;
    setVisibility(pressed);
    return;
  }
  redirect(pressed);
}

function redirect(pressed) {
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
  var pressed = (query.value ?? "").trim();
  setVisibility(pressed);
}
