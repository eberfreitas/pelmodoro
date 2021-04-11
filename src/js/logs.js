import db from "./helpers/db.js";

const insert = cycle => db.cycles.add(cycle);

export default function (app) {
  app.ports.logCycle.subscribe(insert);
}
