import { Howl } from "howler";

const alarmSounds = {
  "wind-chimes": new Howl({ src: "wind-chimes.wav" }),
  "bell": new Howl({ src: "bell.wav" }),
  "alarm-clock": new Howl({ src: "alarm-clock.wav" }),
  "bong": new Howl({ src: "bong.wav" }),
  "relaxing-percussion": new Howl({ src: "relaxing-percussion.wav" }),
  "bird-song": new Howl({ src: "bird-song.wav" })
};

const testSound = sound => {
  if (alarmSounds[sound]) {
    alarmSounds[sound].play();
  }
}

const notify = config => {
  if (config.config.sound && alarmSounds[config.sound]) {
    alarmSounds[config.sound].play();
  }

  if (config.config.browser && permission == "granted") {
    let notif = new Notification(config.msg);
  }
}

const requestNotif = async (app, activate) => {
  if (activate == false) {
    return app.ports.gotBrowserNotifRes.send({ val: false, msg: ""});
  }

  let permission = Notification.permission;

  if (permission == "denied") {
    return app.ports.gotBrowserNotifRes.send({ val: false, msg: "You have blocked browser notifications for this app. Change your in browser settings to allow new notifications."});
  }

  if (permission == "granted") {
    return app.ports.gotBrowserNotifRes.send({ val: true, msg: ""});
  }

  permission = await Notification.requestPermission();

  if (permission == "granted") {
    return app.ports.gotBrowserNotifRes.send({ val: true, msg: ""});
  }

  return app.ports.gotBrowserNotifRes.send({ val: false, msg: "You have blocked browser notifications for this app. Change your browser settings to allow notifications."});
}

export default function (app) {
  app.ports.notify.subscribe(notify);
  app.ports.requestBrowserNotif.subscribe(async activate => await requestNotif(app, activate));
  app.ports.testSound.subscribe(testSound);
}
