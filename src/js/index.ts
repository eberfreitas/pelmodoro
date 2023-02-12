import * as storage from "./helpers/local-storage";
import viewportFix from "./viewport-fix";
import notify from "./notify";
import spotify from "./spotify";
import logs from "./logs";
import settings from "./settings";
import { LocalStoragePayload } from "../globals";

const active = storage.get("active", storage.get("current", {}));
const settings_ = storage.get("settings", {});

const app = window.Elm.Main.init({
  flags: {
    active: active,
    settings: settings_,
    now: Date.now(),
  },
});

app.ports.localStorage.subscribe((payload: LocalStoragePayload) => {
  storage.set(payload["key"], payload["data"]);
});

viewportFix();
notify(app);
spotify(app);
logs(app);
settings(app);

const tickWorker = new Worker("./tick.js");

tickWorker.onmessage = ({ data }) => app.ports.tick.send(data);
