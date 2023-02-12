import { ElmApp } from "../../globals";

export default function (app: ElmApp, msg: unknown): void {
  app.ports.gotFlashMsg.send({ msg: msg });
}
