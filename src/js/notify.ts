import { ElmApp, NotifyPayload } from "../globals";
import alarmSounds from "./helpers/alarm-sounds";

const notify = (config: NotifyPayload) => {
  if (config.config.sound && alarmSounds[config.sound]) {
    alarmSounds[config.sound]?.play();
  }

  const permission = Notification.permission;

  if (config.config.browser && permission == "granted") {
    return new Notification(config.msg);
  }

  return null;
};

export default function(app: ElmApp) {
  app.ports.notify.subscribe(notify);
}
