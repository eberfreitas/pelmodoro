export default function (app, title, msg) {
  app.ports.gotFlashMsg.send({ title: title, msg: msg })
}
