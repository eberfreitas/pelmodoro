import { ElmApp, ToLogPayload } from "../globals";
import db from "./helpers/db";

type Session = {
  interval: {
    type: string;
    secs: number;
  };
  start: number | null;
  end: number | null;
  secs: number | null;
  sentiment: string | null;
};

const insert = (session: Session) => db.cycles.add(session);

const monthlyLogs = (millis: number) => {
  const date = new Date(millis);
  const firstDay = new Date(date.getFullYear(), date.getMonth(), 1, 0, 0, 0);
  const lastDay = new Date(
    date.getFullYear(),
    date.getMonth() + 1,
    0,
    23,
    59,
    59
  );

  return db.cycles
    .where("start")
    .between(firstDay.getTime(), lastDay.getTime())
    .toArray();
};

const fetch = async (app: ElmApp, millis: number) => {
  const logs = await monthlyLogs(millis);

  app.ports.gotFromLog.send({ ts: millis, logs: logs });
};

const updateSentiment = async (time: number, sentiment: string) => {
  const session = await db.cycles.where({ start: time }).toArray();

  if (!session[0] || !session[0].id) {
    return;
  }

  const updated = await db.cycles.update(session[0].id, {
    sentiment: sentiment,
  });

  return updated;
};

export default function(app: ElmApp) {
  app.ports.toLog.subscribe(async (data: ToLogPayload) => {
    switch (data["type"]) {
      case "fetch":
        await fetch(app, data["time"]);
        break;

      case "sentiment":
        await updateSentiment(data["time"], data["sentiment"]);
        break;
    }
  });

  app.ports.log.subscribe(insert);
}
