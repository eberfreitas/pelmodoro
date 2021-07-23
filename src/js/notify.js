import alarmSounds from "./helpers/alarm-sounds.js";

const notify = config => {
  if (config.config.sound && alarmSounds[config.sound]) {
    alarmSounds[config.sound].play();
  }

  let permission = Notification.permission;

  if (config.config.browser && permission == "granted") {
    let notif = new Notification(config.msg);
  }
}


export default function (app) {
  app.ports.notify.subscribe(notify);
}
