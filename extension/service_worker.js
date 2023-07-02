import init, {init_parser} from './wasm_lol/lol_wasm.js';
import {resolve_config} from './config.js';

var parser = null;

function set_parser(config) {
  // TODO: resolve external configs, and so on, store downloadded parts in local cache
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
      resolve_config(JSON.parse(items.config)).then((resolved_config) => {
        return set_parser(JSON.stringify(resolved_config));
      })
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
  for (var i = 0; i < Math.min(20, suggestions.length) ; i++) {
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
      resolve_config(JSON.parse(changes.config.newValue)).then((config) => {
        return set_parser(JSON.stringify(config));
      });
    }
  }
)
