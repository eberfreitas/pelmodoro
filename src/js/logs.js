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

const dailyLogs = millis => {
  const date = new Date(millis);
  const dayStart = new Date(date.getFullYear(), date.getMonth(), date.getDate(), 0, 0, 0);
  const dayEnd = new Date(date.getFullYear(), date.getMonth(), date.getDate(), 23, 59, 59);

  return db.cycles.where("start").between(dayStart.getTime(), dayEnd.getTime()).toArray();
};

const fetch = (app, millis) => {
  const daily = dailyLogs(millis);
  const monthly = monthlyLogs(millis);

  Promise.all([daily, monthly]).then(vals => {
    app.ports.gotStatsLogs.send({ ts: millis, daily: vals[0], monthly: vals[1] });
  });
};

const fetchNav = (app, millis) => monthlyLogs(millis).then(log => app.ports.gotNavLogs.send({ ts: millis, log: log}));

const exportData = () => {
  db.export().then(blob => download(blob, "pelmodoro-data.json", "application/json"));
};


const importData = (app, str) => {
  const blob = new Blob([str]);

  peakImportFile(blob)
    .then(() => {
      db.cycles.clear()
        .then(() => {
          db.import(blob)
            .then(() => setFlash(app, "Success", "Data has been successfully imported."))
            .catch(() => setFlash(app, "Error", "There was an error trying to import the data."));
        })
        .catch(() => setFlash(app, "Error", "There was an error trying to import the data."));
    })
    .catch(() => setFlash(app, "Error", "There was an error trying to import the data."));
}

export default function (app) {
  app.ports.logCycle.subscribe(insert);
  app.ports.fetchLogs.subscribe(millis => fetch(app, millis));
  app.ports.fetchNavLog.subscribe(millis => fetchNav(app, millis));
  app.ports.requestDataExport.subscribe(() => exportData());
  app.ports.importData.subscribe(blob => importData(app, blob));
}
