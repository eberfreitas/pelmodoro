import { Elm } from "./src/Main.elm";

import * as storage from "./src/js/helpers/local-storage.js";
import viewportFix from "./src/js/viewport-fix.js";
import notify from "./src/js/notify.js";
import spotify from "./src/js/spotify.js";
import logs from "./src/js/logs.js";
import settings from "./src/js/settings.js";
import pwa from "./src/js/pwa.js";

const active = storage.get("active", storage.get("current", {}));
const settings_ = storage.get("settings", {});

const app = Elm.Main.init({
  flags: {
    active: active,
    settings: settings_,
    now: Date.now()
  }
});

app.ports.localStorage.subscribe(payload => storage.get(payload["key"], payload["data"]));

// viewportFix();
//notify(app);
spotify(app);
logs(app);
settings(app);
// pwa();

// const tickWorker = new Worker('./src/js/tick.js');

// tickWorker.onmessage = ({ data }) => app.ports.tick.send(data);
