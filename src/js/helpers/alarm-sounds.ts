import { Howl } from "howler";

const alarmSounds: Record<string, Howl> = {
  "wind-chimes": new Howl({ src: "wind-chimes.wav" }),
  bell: new Howl({ src: "bell.wav" }),
  "alarm-clock": new Howl({ src: "alarm-clock.wav" }),
  bong: new Howl({ src: "bong.wav" }),
  "relaxing-percussion": new Howl({ src: "relaxing-percussion.wav" }),
  "bird-song": new Howl({ src: "bird-song.wav" }),
} as const;

export default alarmSounds;
