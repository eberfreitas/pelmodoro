import db from "./helpers/db.js";

const insert = cycle => db.cycles.add(cycle);

const monthlyLogs = millis => {
  const date = new Date(millis);
  const firstDay = new Date(date.getFullYear(), date.getMonth(), 1, 0, 0, 0);
  const lastDay = new Date(date.getFullYear(), date.getMonth() + 1, 0, 23, 59, 59);

  return db.cycles.where("start").between(firstDay.getTime(), lastDay.getTime()).toArray();
};

const fetch = async (app, millis) => {
  const logs = await monthlyLogs(millis);

  app.ports.gotFromLog.send({ ts: millis, logs: logs });
};




const updateCycle = async data => {
  const cycle = await db.cycles.where({ start: data[0]}).toArray();

  if (!cycle[0]) {
    return;
  }

  const updated = await db.cycles.update(cycle[0].id, { sentiment: data[1] });

  return updated;
}


export default function (app) {
  app.ports.toLog.subscribe(async data => {
    switch (data["type"]) {
      case "fetch":
        await fetch(app, data["time"]);
        break;
    }
  });


  // app.ports.logCycle.subscribe(insert);
  // app.ports.fetchLogs.subscribe(async millis => await fetch(app, millis));
  // app.ports.requestDataExport.subscribe(async () => await exportData());
  // app.ports.importData.subscribe(async blob => await importData(app, blob));
  // app.ports.updateCycle.subscribe(async data => await updateCycle(data));
  // app.ports.clearLogs.subscribe(clearLogs);
}
