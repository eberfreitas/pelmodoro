import alarmSounds from "./helpers/alarm-sounds.js";


const notify = config => {
  if (config.config.sound && alarmSounds[config.sound]) {
    alarmSounds[config.sound].play();
  }

  if (config.config.browser && permission == "granted") {
    let notif = new Notification(config.msg);
  }
}


export default function (app) {
  app.ports.toSettings.subscribe(async data => {
    switch (data["type"]) {
      case "testAlarm":
        testSound(data["data"]);
        break;

      case "browserPermission":
        await requestNotif(app, data["data"]);
        break;
    }
  });

  // app.ports.notify.subscribe(notify);
  // app.ports.requestBrowserNotif.subscribe(async activate => await requestNotif(app, activate));
  // app.ports.testSound.subscribe(testSound);
}
