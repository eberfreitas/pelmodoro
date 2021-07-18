import { Howl } from "howler";

const alarmSounds = {
  "wind-chimes": new Howl({ src: "wind-chimes.wav" })
};

export default function (app) {
  app.ports.notify.subscribe(config => {
    if (config.config.sound && alarmSounds[config.sound]) {
      alarmSounds[config.sound].play();
    }

    if (config.config.browser && permission == "granted") {
      let notif = new Notification(config.msg);
    }
  });

  app.ports.requestBrowserNotif.subscribe(async activate => {
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
  });
}
