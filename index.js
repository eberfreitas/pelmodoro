import { Elm } from "./src/Main.elm";
import { Howl } from "howler";

const current = JSON.parse(window.localStorage.getItem("current")) || {};
const settings = JSON.parse(window.localStorage.getItem("settings")) || {};

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: {
    current: current,
    settings: settings
  }
});

const sound = new Howl({ src: "wind-chimes.wav" });

app.ports.notify.subscribe(() => sound.play());
app.ports.persistCurrent.subscribe(current => window.localStorage.setItem("current", JSON.stringify(current)));
app.ports.persistSettings.subscribe(settings => window.localStorage.setItem("settings", JSON.stringify(settings)));
