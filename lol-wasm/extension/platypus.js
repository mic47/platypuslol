import init, {init_parser} from './wasm_lol/lol_wasm.js';

var parser = null;

init().then(() => {
  parser = init_parser(JSON.stringify({
    "substitutions": {
      "product": [
        {
          "type": "document",
          "url_part": "document"
        },
        {
          "type": "sheet",
          "url_part": "spreadsheets"
        },
        {
          "type": "presentation",
          "url_part": "presentation"
        },
        {
          "type": "drawing",
          "url_part": "drawings"
        }
      ]
    },
    "redirects": [
      {
        "query": "mail",
        "link": "https://mail.google.com/mail/u/0"
      },
      {
        "query": "search mail {query}",
        "link": "https://mail.google.com/mail/u/0/#search/{query}"
      },
      {
        "query": "docs",
        "link": "https://docs.google.com/document/u/0/"
      },
      {
        "query": "calendar",
        "link": "https://calendar.google.com/calendar/b/1/r"
      },
      {
        "query": "bing {query:query}",
        "link": "https://www.bing.com/search?q={query}"
      },
      {
        "query": "obi {query}",
        "link": "https://www.obi.sk/search/{query}"
      },
      {
        "query": "ikea {query}",
        "link": "https://www.ikea.com/sk/sk/search/products/?q={query}"
      },
      {
        "query": "search google docs {query}",
        "link": "https://docs.google.com/document/u/0/?q={query}&tgif=d"
      },
      {
        "query": "create google {:subst:product:type}",
        "link": "https://docs.google.com/{product:url_part}/u/0/create"
      },
      {
        "query": "local elasticsearch {query}",
        "link": "http://localhost:9200/_search?q={query}"
      },
      {
        "query": "personal code search {query}",
        "link": "http://localhost:5074/search?q={query}"
      },
      {
        "query": "external code search {query}",
        "link": "http://localhost:5048/search?q={query}"
      },
      {
        "query": "prod elasticsearch {query}",
        "link": "http://localhost:9201/_search?q={query}"
      },
      {
        "query": "pull requests",
        "link": "https://github.com/pulls"
      },
      {
        "query": "google cloud storage {query}",
        "link": "https://console.cloud.google.com/storage/browser/{query}?authuser=0"
      },
      {
        "query": "pip {query}",
        "link": "https://pypi.org/search/?q={query}"
      },
      {
        "query": "python documentation {query}",
        "link": "https://docs.python.org/3/library/{query}.html"
      }
    ]
  }));
})

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
      description: escapeHTML(sug["text"]),
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

