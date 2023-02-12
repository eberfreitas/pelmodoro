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

type NotifyConfig = {
  sound: string;
  msg: string;
  config: Notifications;
};

interface NotifySubscribe {
  (config: NotifyConfig): void;
}

export type ToLogPayload =
  | { type: "fetch"; time: number }
  | { type: "sentiment"; time: number; sentiment: string };

interface ToLogSubscribe {
  (payload: ToLogPayload): void;
}

export interface ElmApp {
  ports: {
    localStorage: {
      subscribe(LocalStorageSubscribe): void;
    };
    tick: {
      send(unknown): void;
    };
    gotFlashMsg: {
      send(unknown): void;
    };
    notify: {
      subscribe(NotifySubscribe): void;
    };
    gotFromLog: {
      send(unknown): void;
    };
    toLog: {
      subscribe(ToLogSubscribe): void;
    };
    log: {
      subscribe(unknown): void;
    }
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
