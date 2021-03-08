import { Elm } from "./src/Main.elm";
import { Howl } from "howler";


const app = Elm.Main.init({ node: document.getElementById("root") });

const sound = new Howl({ src: "wind-chimes.wav" });

app.ports.playSound.subscribe(() => sound.play());
