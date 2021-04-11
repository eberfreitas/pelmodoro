import Dexie from "dexie";

const db = new Dexie("DB");

db.version(1).stores({
  cycles: "++id,start"
});

export default db;
