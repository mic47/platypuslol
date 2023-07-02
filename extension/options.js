import init, {init_parser} from './wasm_lol/lol_wasm.js';
import {resolve_config} from './config.js';

init()

// Saves options to chrome.storage
const saveOptions = () => {
  try {
    const do_stuff = (json_config) => resolve_config(JSON.parse(json_config)).then((resolved_config) => {
      init_parser(JSON.stringify(resolved_config));
      const config = JSON.parse(json_config);
      return chrome.storage.sync.set(
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
    });
    const json_config = document.getElementById('config').value;
    if (json_config == "reset") {
      fetch(chrome.runtime.getURL('commands.json'))
        .then((resp) => resp.json())
      .then((defaultConfig) => {
        return do_stuff(JSON.stringify(defaultConfig));
      }).then((_) => restoreOptions());
    } else {
      do_stuff(json_config)
    }


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
  chrome.storage.local.get(
    ["external_configurations"],
    (items) => {
      var element = document.getElementById('remote_configs');
      if (items.external_configurations !== undefined && items.external_configurations !== null) {
        const as_text = JSON.stringify(JSON.parse(items.external_configurations), null, 2);
        element.innerHTML = as_text;
      } else {
        element.innerHTML = "";
      }
    }
  )
};

document.addEventListener('DOMContentLoaded', restoreOptions);
document.getElementById('save').addEventListener('click', saveOptions);
