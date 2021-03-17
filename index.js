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

// Full height display hack: https://css-tricks.com/the-trick-to-viewport-units-on-mobile/

// First we get the viewport height and we multiple it by 1% to get a value for a vh unit
let vh = window.innerHeight * 0.01;

// Then we set the value in the --vh custom property to the root of the document
document.documentElement.style.setProperty('--vh', `${vh}px`);

// We listen to the resize event
window.addEventListener('resize', () => {
   // We execute the same script as before
   let vh = window.innerHeight * 0.01;
   document.documentElement.style.setProperty('--vh', `${vh}px`);
});
