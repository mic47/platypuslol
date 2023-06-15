chrome.omnibox.onInputStarted.addListener(() => {
  console.log('Input started');
})

chrome.omnibox.onInputChanged.addListener((text, suggest) => {
  console.log("Changed", text);
  console.log("Changed sug", suggest);
  suggest([{
    content: text + " lol",
    deletable: false,
    description: "Foo Bar"
  }, {
    content: text + "platypus",
    deletable: false,
    description: "Platypus bar"
  }]);

})

chrome.omnibox.onInputEntered.addListener((text) => {
  console.log("On input entered", text);
  const newURL = 'https://www.google.com/search?q=' + encodeURIComponent(text);
  chrome.tabs.create({ url: newURL });
});

