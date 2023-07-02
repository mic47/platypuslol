
export function resolve_config(config) {
  return chrome.storage.local.get({"external_configurations": '{}'}).then((json) => {
    let old_external_config = JSON.parse(json["external_configurations"]);
    if (config["external_configurations"] === undefined) {
      config["external_configurations"] = {}
    }
    const external_configs = config["external_configurations"];
    let to_fetch = [];
    for (const k in external_configs) {
      const value = external_configs[k];
      if (!value["enabled"]) {
        continue
      }
      if (k.startsWith("builtin://")) {
        const url = chrome.runtime.getURL(k.replace("builtin://", ""))
        to_fetch.push(fetch(url).then((result) => result.json()).then((result) => [k, result]).catch(
          (_) => [k, old_external_config[k]])
        )
      } else if (k.startsWith("local://")) {
        console.log('Local files are not supported', k)
      } else {
        to_fetch.push(fetch(k, {"mode": "no-cors"}).then((result) => result.json()).then((result) => [k, result]).catch(
          (_) => [k, old_external_config[k]])
        )
      }
    }
    return Promise.all(to_fetch).then((external) => {
      let stored={};
      for (let index = 0; index < external.length; index++) {
        const [key, econfig] = external[index];
        if (config === null || econfig === undefined) {
          continue
        }
        config["external_configurations"][key]["config"] = econfig;
        stored[key] = econfig;
      }
      return chrome.storage.local.set({external_configurations: JSON.stringify(stored)}).catch((_) => null).then((_) => config)
    });
  })
}

