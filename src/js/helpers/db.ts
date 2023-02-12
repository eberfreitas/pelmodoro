import Dexie from "dexie";
import "dexie-export-import";

interface Cycles {
  id?: number;
  start: number | null;
  end: number | null;
  secs: number | null;
  sentiment: string | null;
  interval?: {
    type: string;
    secs: number;
  };
}

class Database extends Dexie {
  cycles!: Dexie.Table<Cycles, number>;

  constructor() {
    super("DB");

    this.version(1).stores({
      cycles: "++id, start",
    });
  }
}

export default new Database();
