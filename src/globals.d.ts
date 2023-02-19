import { SpotifyDef } from "./spotify";

export interface LocalStoragePayload {
  key: string;
  data: unknown;
}

type LocalStorageSubscribe = (payload: LocalStoragePayload) => void;

interface Notifications {
  inapp: boolean;
  sound: boolean;
  browser: boolean;
}

interface NotifyPayload {
  sound: string;
  msg: string;
  config: Notifications;
}

type NotifySubscribe = (config: NotifyPayload) => void;

export type ToLogPayload =
  | { type: "fetch"; time: number }
  | { type: "sentiment"; time: number; sentiment: string };

type ToLogSubscribe = (payload: ToLogPayload) => void;

export interface LogPayload {
  interval: {
    type: string;
    secs: number;
  };
  start: number | null;
  end: number | null;
  secs: number | null;
  sentiment: string | null;
}

type LogSubscribe = (payload: LogPayload) => void;

export type ToSettingsPayload =
  | { type: "requestExport" }
  | { type: "import"; data: string }
  | { type: "delete" }
  | { type: "testAlarm"; data: string }
  | { type: "browserPermission"; data: boolean };

type ToSettingsSubscribe = (payload: ToSettingsSubscribe) => void;

export type ToSpotifyPayload =
  | { type: "play"; url: string }
  | { type: "pause" }
  | { type: "refresh" }
  | { type: "disconnect" };

type ToSpotifySubscribe = (payload: ToSpotifyPayload) => void;

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
    toSpotify: {
      subscribe(ToSpotifySubscribe): void;
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
    gotFromSpotify: {
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
