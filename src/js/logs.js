import db from "./helpers/db.js";

const insert = cycle => {
  db.cycles.add(cycle).catch(console.log);
}

export default function (app) {
  app.ports.logCycle.subscribe(insert);
}
