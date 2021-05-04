import { Elm } from "./src/Main.elm";

import * as storage from "./src/js/helpers/local-storage.js";
import viewportFix from "./src/js/viewport-fix.js";
import notify from "./src/js/notify.js";
import spotify from "./src/js/spotify.js";
import logs from "./src/js/logs.js";

const current = storage.get("current", {});
const settings = storage.get("settings", {});

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    current: current,
    settings: settings
  }
});

app.ports.persistCurrent.subscribe(current => storage.set("current", current));
app.ports.persistSettings.subscribe(settings => storage.set("settings", settings));

viewportFix();
notify(app);
spotify(app);
logs(app);

const tickWorker = new Worker('./src/js/tick.js');

tickWorker.onmessage = ({ data }) => app.ports.tick.send(data);
