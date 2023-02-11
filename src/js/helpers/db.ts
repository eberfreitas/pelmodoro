import Dexie from "dexie";
import "dexie-export-import";

const db = new Dexie("DB");

db.version(1).stores({ cycles: "++id,start" });

export default db;
