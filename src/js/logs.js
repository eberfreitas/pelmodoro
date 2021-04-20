import db from "./helpers/db.js";

const insert = cycle => db.cycles.add(cycle);

const fetch = (app, millis) => {
  const date = new Date(millis);
  const start = new Date(date.getFullYear(), date.getMonth(), 1, 0, 0, 0);
  const finish = new Date(date.getFullYear(), date.getMonth() + 1, 0, 23, 59, 59);

  db.cycles.where("start")
    .between(start.getTime(), finish.getTime())
    .toArray()
    .then((log) => app.ports.gotStatsLogs.send({ ts: millis, log: log }));
};

export default function (app) {
  app.ports.logCycle.subscribe(insert);
  app.ports.fetchLogs.subscribe((millis) => fetch(app, millis));
}
