fill_suggestions();
function fill_suggestions() {
	chrome.storage.local.get(["default_suggestion"]).then(({default_suggestion}) => {
		const suggestions = JSON.parse(default_suggestion)
		const commands = document.getElementById("commands")
		suggestions.sort(compare_suggestions);
		const ul = document.createElement("ul");
		var visited = new Set();
		for (var i = 0; i < suggestions.length; i ++ ) {
			const suggestion = suggestions[i];
			const hash = JSON.stringify(suggestion);
			if (visited.has(hash)) {
				continue;
			}
			visited.add(hash);
			const li = document.createElement("li");
			const a = document.createElement("a");
			a.textContent = suggestion["text"]
			a.setAttribute("href", suggestion["link"]);
			a.addEventListener('click', go_to_link);
			li.appendChild(a);
			ul.appendChild(li);
		}
		commands.appendChild(ul);
	})
}

function compare_suggestions(a, b) {
	const at = a["text"];
	const bt = b["text"];
	if (at < bt) {
		return -1;
	} else if (at > bt) {
		return 1;
	} else {
		return 0;
	}
}

function go_to_link(element) {
	const link = element.target.href;
  let queryOptions = { active: true, lastFocusedWindow: true };
  chrome.tabs.query(queryOptions).then(([tab]) => {
    if (tab != undefined) {
      chrome.tabs.update(tab.tabId, { url: encodeURI(link) });
    } else {
      chrome.tabs.create({ url: encodeURI(link) });
    }
  });
}
