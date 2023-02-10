import * as storage from "./helpers/local-storage.js";
import viewportFix from "./viewport-fix.js";
import notify from "./notify.js";
import spotify from "./spotify.js";
import logs from "./logs.js";
import settings from "./settings.js";

const active = storage.get("active", storage.get("current", {}));
const settings_ = storage.get("settings", {});

const app = window.Elm.Main.init({
  flags: {
    active: active,
    settings: settings_,
    now: Date.now(),
  },
});

app.ports.localStorage.subscribe((payload) =>
  storage.set(payload["key"], payload["data"])
);

viewportFix();
notify(app);
spotify(app);
logs(app);
settings(app);

const tickWorker = new Worker("./src/js/tick.js");

tickWorker.onmessage = ({ data }) => app.ports.tick.send(data);
