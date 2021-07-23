import db from "./helpers/db.js";

const insert = session => db.cycles.add(session);

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

const updateSentiment = async (time, sentiment) => {
  const session = await db.cycles.where({ start: time}).toArray();

  if (!session[0]) {
    return;
  }

  const updated = await db.cycles.update(session[0].id, { sentiment: sentiment });

  return updated;
}

export default function (app) {
  app.ports.toLog.subscribe(async data => {
    switch (data["type"]) {
      case "fetch":
        await fetch(app, data["time"]);
        break;

      case "sentiment":
        await updateSentiment(data["time"], data["sentiment"]);
        break;
    }
  });

  // app.ports.logCycle.subscribe(insert);
}
