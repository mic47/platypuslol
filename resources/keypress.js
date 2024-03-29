function reset(event) {
  setQuery("");
}

function setQuery(queryString) {
  var query = document.getElementById('query');
  if (query == null) {
    return;
  }
  query.value= queryString;
  setVisibility(queryString);
}

function expandState() {
  var expand = false;
  if (document.getElementById('expand')?.checked) {
    expand = true
  }
  return expand;
}

function flipExpandState() {
  let item = document.getElementById('expand');
  if (item == null) {
    return
  }
  item.checked = !item.checked;
  redraw();
}

function setVisibility(pressed) {
  let expand = expandState();
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
        el.style.display = 'none';
        return
      }
      if (!par.startsWith(onparent) && !expand) {
        el.style.display = 'none';
        return
      }
      el.style.display = '';
    })
}

function onKeyPress(event) {
  if (event.key == 'e') {
    flipExpandState();
    return;
  }
  var query = document.getElementById('query');
  if (query == null) {
    return
  }
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
    .flatMap((tag) => Array.from(tag.children))
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

function redraw() {
  var query = document.getElementById('query');
  var pressed = (query?.value ?? "").trim();
  setVisibility(pressed);
}

function onLoad() {
  Array.from(document.getElementsByClassName("list_commands"))
    .forEach((element) => {
      element.addEventListener('keydown', onKeyPress);
    });
  window.addEventListener('pageshow', reset);
  window.addEventListener('pagehide', reset);
  window.addEventListener('pagehide', reset);
  window.addEventListener('beforeunload', reset);
  redraw();
}

function flipVisibility(id) {
  console.log("flipping", id);
  var element = document.getElementById(id);
  if (element == null || element == undefined) {
    return;
  }
  if (element.style.display == 'none') {
    element.style.display = '';
  } else {
    element.style.display = 'none';
  }
}
