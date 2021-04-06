import { Howl } from "howler";

const alarmSounds = {
  "wind-chimes": new Howl({ src: "wind-chimes.wav" })
};

export default function (app) {
  app.ports.notify.subscribe(() => alarmSounds["wind-chimes"].play());
}
