import { peakImportFile } from "dexie-export-import";
import download from "downloadjs";
import { ElmApp, ToSettingsPayload } from "../globals";
import alarmSounds from "./helpers/alarm-sounds.js";
import db from "./helpers/db";
import setFlash from "./helpers/flash.js";

const testSound = (sound: string): void => {
  if (alarmSounds[sound]) {
    alarmSounds[sound]?.play();
  }
};

const requestNotif = async (app: ElmApp, activate: boolean) => {
  if (!activate) {
    return app.ports.gotFromSettings.send({ val: false, msg: "" });
  }

  let permission = Notification.permission;

  if (permission == "denied") {
    return app.ports.gotFromSettings.send({
      val: false,
      msg: "You have blocked browser notifications for this app. Change your browser settings to allow new notifications.",
    });
  }

  if (permission == "granted") {
    return app.ports.gotFromSettings.send({ val: true, msg: "" });
  }

  permission = await Notification.requestPermission();

  if (permission == "granted") {
    return app.ports.gotFromSettings.send({ val: true, msg: "" });
  }

  return app.ports.gotFromSettings.send({
    val: false,
    msg: "You have blocked browser notifications for this app. Change your browser settings to allow notifications.",
  });
};

const importData = async (app: ElmApp, str: string) => {
  const blob = new Blob([str]);

  try {
    await peakImportFile(blob);
    await db.cycles.clear();
    await db.import(blob);

    setFlash(app, "Data has been successfully imported.");
  } catch (e) {
    setFlash(app, "There was an error trying to import the data.");
  }
};

const exportData = async () => {
  const blob = await db.export();

  download(blob, "pelmodoro-data.json", "application/json");
};

const clearLogs = () => {
  const confirmed = confirm(
    "Are you sure you wanna delete all log entries? This step is irreversible!"
  );

  if (!confirmed) {
    return;
  }

  void db.cycles.clear();
};

export default function(app: ElmApp) {
  app.ports.toSettings.subscribe(async (data: ToSettingsPayload) => {
    switch (data.type) {
      case "testAlarm":
        testSound(data.data);
        break;

      case "browserPermission":
        await requestNotif(app, data.data);
        break;

      case "import":
        await importData(app, data.data);
        break;

      case "requestExport":
        await exportData();
        break;

      case "delete":
        clearLogs();
        break;
    }
  });
}
