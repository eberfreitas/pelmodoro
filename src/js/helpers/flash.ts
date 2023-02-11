export default function (app, msg) {
  app.ports.gotFlashMsg.send({ msg: msg });
}
