import init, {init_parser} from './wasm_lol/lol_wasm.js';

var parser = null;

function set_parser(config) {
  parser = init_parser(config);
	const default_suggestion = parser.suggest("");
	chrome.storage.local.set({default_suggestion: default_suggestion});
}

init().then(() => {
  fetch(chrome.runtime.getURL('commands.json'))
    .then((resp) => resp.json())
  .then((defaultConfig) => {
    chrome.storage.sync.get(
      { config: JSON.stringify(defaultConfig) },
    ).then((items) => {
			set_parser(items.config);
    });
  });
});

chrome.omnibox.onInputStarted.addListener(() => {
})

function escapeHTML(string){
  // Just escape everything, safest way to sanitize
  return string.replace(/[^]/g, function(character) {
    return"&#"+character.charCodeAt(0)+";"
  })
}

chrome.omnibox.onInputChanged.addListener((text, send_suggestion) => {
  let sug = parser.suggest(text);
  let suggestions = JSON.parse(sug);
  var output = [];
  for (var i = 0; i < suggestions.length ; i++) {
    let sug = suggestions[i];
    output.push({
      content: sug["text"],
      deletable: false,
      description: escapeHTML(sug["text"]) + " ➡️  " + escapeHTML(sug["link"]),
    });
  }
  send_suggestion(output);
})

chrome.omnibox.onInputEntered.addListener((text) => {
  let link = parser.redirect(text);
  if (link == undefined || link == null) {
    return
  }
  let queryOptions = { active: true, lastFocusedWindow: true };
  chrome.tabs.query(queryOptions).then(([tab]) => {
    if (tab != undefined) {
      chrome.tabs.update(tab.tabId, { url: encodeURI(link) });
    } else {
      chrome.tabs.create({ url: encodeURI(link) });
    }
  });
});

chrome.storage.onChanged.addListener(
  (changes, areaName) => {
    if (areaName == "sync" && changes.config != undefined && changes.config.newValue != undefined) {
			set_parser(changes.config.newValue);
    }
  }
)
