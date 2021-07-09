import NoSleep from "nosleep.js";

const sw = "sw.js";

export default function () {
  navigator.serviceWorker.register(sw);

  const noSleep = new NoSleep();

  document.addEventListener("click", function enableNoSleep() {
    document.removeEventListener("click", enableNoSleep, false);
    noSleep.enable();
  }, false);
}
