
// Saves options to chrome.storage
const saveOptions = () => {
  try {
    const config = JSON.parse(document.getElementById('config').value);
    chrome.storage.sync.set(
      { config: JSON.stringify(config) },
      () => {
        // Update status to let user know options were saved.
        const status = document.getElementById('status');
        status.textContent = 'Options saved.';
        setTimeout(() => {
          status.textContent = '';
        }, 750);
      }
    );
  } catch (error) {
      const status = document.getElementById('status');
      status.textContent = 'Unable to parse content ' + error;
    return;
  }
};

// Restores select box and checkbox state using the preferences
// stored in chrome.storage.
const restoreOptions = () => {
  chrome.storage.sync.get(
    ["config"],
    (items) => {
      var element = document.getElementById('config');
      const as_text = JSON.stringify(JSON.parse(items.config), null, 2);
      element.value = as_text;
      element.rows = Math.max(as_text.split("\n").length, 50);
    }
  );
};

document.addEventListener('DOMContentLoaded', restoreOptions);
document.getElementById('save').addEventListener('click', saveOptions);
