import db from "./helpers/db.js";
import download from "downloadjs";
import { peakImportFile } from "dexie-export-import";

import setFlash from "./helpers/flash.js";

const insert = cycle => db.cycles.add(cycle);

const monthlyLogs = millis => {
  const date = new Date(millis);
  const firstDay = new Date(date.getFullYear(), date.getMonth(), 1, 0, 0, 0);
  const lastDay = new Date(date.getFullYear(), date.getMonth() + 1, 0, 23, 59, 59);

  return db.cycles.where("start").between(firstDay.getTime(), lastDay.getTime()).toArray();
};

const fetch = async (app, millis) => {
  const logs = await monthlyLogs(millis);

  app.ports.gotStatsLogs.send({ ts: millis, logs: logs });
};

const exportData = async () => {
  const blob = await db.export();

  download(blob, "pelmodoro-data.json", "application/json")
};


const importData = async (app, str) => {
  const blob = new Blob([str]);

  try {
    await peakImportFile(blob);
    await db.cycles.clear();
    await db.import(blob);

    setFlash(app, "Success", "Data has been successfully imported.")
  } catch (e) {
    setFlash(app, "Error", "There was an error trying to import the data.")
  }
}

const updateCycle = async data => {
  const cycle = await db.cycles.where({ start: data[0]}).toArray();

  if (!cycle[0]) {
    return;
  }

  const updated = await db.cycles.update(cycle[0].id, { sentiment: data[1] });

  return updated;
}

const clearLogs = () => {
  const confirmed = confirm("Are you sure you wanna delete all log entries? This step is irreversible!");

  if (!confirmed) {
    return;
  }

  db.cycles.clear();
}

export default function (app) {
  app.ports.logCycle.subscribe(insert);
  app.ports.fetchLogs.subscribe(async millis => await fetch(app, millis));
  app.ports.requestDataExport.subscribe(async () => await exportData());
  app.ports.importData.subscribe(async blob => await importData(app, blob));
  app.ports.updateCycle.subscribe(async data => await updateCycle(data));
  app.ports.clearLogs.subscribe(clearLogs);
}
