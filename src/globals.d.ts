import { SpotifyDef } from "./spotify";

export type LocalStoragePayload = {
  key: string;
  data: unknown;
};

interface LocalStorageSubscribe {
  (payload: LocalStoragePayload): void;
}

type Notifications = {
  inapp: boolean;
  sound: boolean;
  browser: boolean;
};

type NotifyPayload = {
  sound: string;
  msg: string;
  config: Notifications;
};

interface NotifySubscribe {
  (config: NotifyPayload): void;
}

export type ToLogPayload =
  | { type: "fetch"; time: number }
  | { type: "sentiment"; time: number; sentiment: string };

interface ToLogSubscribe {
  (payload: ToLogPayload): void;
}

export type LogPayload = {
  interval: {
    type: string;
    secs: number;
  };
  start: number | null;
  end: number | null;
  secs: number | null;
  sentiment: string | null;
};

interface LogSubscribe {
  (payload: LogPayload): void;
}

export type ToSettingsPayload =
  | { type: "requestExport" }
  | { type: "import"; data: string }
  | { type: "delete" }
  | { type: "testAlarm"; data: string }
  | { type: "browserPermission"; data: boolean };

interface ToSettingsSubscribe {
  (payload: ToSettingsSubscribe): void;
}

export interface ElmApp {
  ports: {
    // ports
    localStorage: {
      subscribe(LocalStorageSubscribe): void;
    };
    notify: {
      subscribe(NotifySubscribe): void;
    };
    toLog: {
      subscribe(ToLogSubscribe): void;
    };
    log: {
      subscribe(LogSubscribe): void;
    };
    toSettings: {
      subscribe(ToSettingsSubscribe): void;
    };

    // subscriptions
    tick: {
      send(unknown): void;
    };
    gotFlashMsg: {
      send(unknown): void;
    };
    gotFromLog: {
      send(unknown): void;
    };
    gotFromSettings: {
      send(unknown): void;
    };
  };
}

interface ElmAppConstructor {
  Main: {
    init(unknown): ElmApp;
  };
}

declare global {
  interface Window {
    spotifyPlayerLoaded: boolean;
    spotify: SpotifyDef;
    Elm: ElmAppConstructor;
  }
}
