import { Howl } from "howler";

const alarmSounds = {
  "wind-chimes": new Howl({ src: "wind-chimes.wav" })
};

export default function (app) {
  let realPermission = Notification.permission;

  app.ports.notify.subscribe(config => {
    console.log(config);

    if (config.config.sound) {
      alarmSounds[config.sound].play();
    }

    if (config.config.browser && realPermission == "granted") {
      let notif = new Notification(config.msg);
    }
  });

  app.ports.requestBrowserNotif.subscribe(activate => {
    if (activate == false) {
      return app.ports.gotBrowserNotifRes.send({ val: false, msg: ""});
    }

    if (realPermission == "denied") {
      return app.ports.gotBrowserNotifRes.send({ val: false, msg: "You have blocked browser notifications for this app. Change your in browser settings to allow new notifications."});
    }

    if (realPermission == "granted") {
      return app.ports.gotBrowserNotifRes.send({ val: true, msg: ""});
    }

    Notification.requestPermission()
      .then(permission => {
        realPermission = permission;

        if (permission == "granted") {
          app.ports.gotBrowserNotifRes.send({ val: true, msg: ""});
          return;
        }

        app.ports.gotBrowserNotifRes.send({ val: false, msg: "You have blocked browser notifications for this app. Change your in browser settings to allow new notifications."});
      })
  });
}
