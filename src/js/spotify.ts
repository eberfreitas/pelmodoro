import randomString from "crypto-random-string";
import pkceChallenge from "pkce-challenge";
import { ElmApp, ToSpotifyPayload } from "../globals";
import {
  AuthData,
  authData as authDataDecoder,
  decodeWith,
  playbackState,
  spotifyApiToken,
  SpotifyApiToken,
  SpotifyConnectData,
  spotifyConnectData,
  spotifyPlaylist,
} from "./decoders";
import setFlash from "./helpers/flash";
import * as storage from "./helpers/local-storage";

const clientId = import.meta.env.VITE_SPOTIFY_CLIENT_ID;
const redirectUri = import.meta.env.VITE_SPOTIFY_REDIRECT_URL;
const redirectUrl = new URL(redirectUri);

let player: Spotify.Player | undefined;

export interface SpotifyDef {
  connected: boolean;
  canPlay: boolean;
  playing: boolean;
  deviceId: string | null;
}

interface Playlist {
  uri: string;
  title: string;
}

const spotify: SpotifyDef = {
  connected: false,
  canPlay: false,
  playing: false,
  deviceId: null,
};

window.spotify = spotify;

const connectData = (): SpotifyConnectData => {
  const pkce = pkceChallenge(128);
  const state = randomString({ length: 16, type: "url-safe" });

  // https://developer.spotify.com/documentation/general/guides/scopes/
  const scopes = [
    // Spotify Connect
    "user-read-playback-state",
    "user-modify-playback-state",
    "user-read-currently-playing",

    // Playback
    "app-remote-control",
    "streaming",

    // Playlists
    "playlist-read-private",
    "playlist-read-collaborative",

    // Users
    "user-read-email",
    "user-read-private",
  ];

  const url =
    `https://accounts.spotify.com/authorize?client_id=${clientId}` +
    `&response_type=code` +
    `&redirect_uri=${encodeURI(redirectUri)}` +
    `&state=${state}` +
    `&code_challenge_method=S256` +
    `&code_challenge=${pkce.code_challenge}` +
    `&scope=${scopes.join(",")}`;

  const data = { ...pkce, state, url };

  storage.set("spotifyConnectData", data);

  return data;
};

const processAuthData = (apiTokeData: SpotifyApiToken): AuthData => {
  const now = Date.now();
  const expiresAt = now + apiTokeData.expires_in * 1000;
  const authData: AuthData = { ...apiTokeData, expires_at: expiresAt };

  storage.set("spotifyAuthData", authData);

  return authData;
};

const promiseByStatus = (res: Response): Promise<unknown> => {
  if (res.status != 200) {
    return Promise.reject(null);
  } else {
    return res.json();
  }
};

const getPlaylists = (token: string): Promise<Playlist[]> => {
  return fetch("https://api.spotify.com/v1/me/playlists?limit=50", {
    headers: { Authorization: `Bearer ${token}` },
  })
    .then(promiseByStatus)
    .then(spotifyPlaylist)
    .then((data) => {
      return Promise.resolve(
        data.items.map((item) => {
          return { uri: item.uri, title: item.name };
        })
      );
    });
};

const authRequest = (body: URLSearchParams): Promise<AuthData> => {
  return fetch("https://accounts.spotify.com/api/token", {
    method: "POST",
    body: body,
  })
    .then(promiseByStatus)
    .then(spotifyApiToken)
    .then((authData) => {
      window.spotify.connected = true;

      return Promise.resolve(processAuthData(authData));
    });
};

const refreshAuthToken = (token: string): Promise<AuthData> => {
  const body = new URLSearchParams();

  body.append("client_id", clientId);
  body.append("grant_type", "refresh_token");
  body.append("refresh_token", token);

  return authRequest(body);
};

const getAuthToken = (code: string, state: string): Promise<AuthData> => {
  const connectData = spotifyConnectData(storage.get("spotifyConnectData", {}));

  if (state != connectData.state) {
    // @TODO: Replace this with a `Result`
    throw new Error("Incorrect return state");
  }

  const body = new URLSearchParams();

  body.append("client_id", clientId);
  body.append("grant_type", "authorization_code");
  body.append("code", code);
  body.append("redirect_uri", redirectUri);
  body.append("code_verifier", connectData.code_verifier);

  return authRequest(body);
};

const notConnected = (app: ElmApp, flash = true): void => {
  app.ports.gotFromSpotify.send({
    type: "notconnected",
    url: connectData().url,
  });

  if (flash) {
    setFlash(app, "Your Spotify account is disconnected.");
  }
};

const connectionError = (app: ElmApp): void => {
  app.ports.gotFromSpotify.send({
    type: "connectionerror",
    url: connectData().url,
  });
  setFlash(
    app,
    "There was an error trying to connect to your Spotify account. Note: you need a Premium account to connect."
  );
};

const connected = (app: ElmApp, playlists: Playlist[], flash = true): void => {
  app.ports.gotFromSpotify.send({
    type: "connected",
    playlists: playlists,
    playlist: null,
  });

  if (flash) {
    setFlash(app, "Your Spotify account is connected.");
  }
};

const setupPlaylists = (app: ElmApp, token: string, flash: boolean): void => {
  getPlaylists(token)
    .then((playlists) => connected(app, playlists, flash))
    .catch(() => notConnected(app, flash));
};

const connectionCallback = (app: ElmApp, code: string, state: string): void => {
  const response = getAuthToken(code, state);

  response
    .then((data) => init(app, data.access_token, true))
    .catch(() => connectionError(app));

  history.pushState({}, "", redirectUrl.pathname);
};

const initPlayer = (app: ElmApp, token: string, retries: number): void => {
  if (retries > 9) {
    return;
  }

  if (
    !window.spotifyPlayerLoaded ||
    window.spotify.connected == false
  ) {
    setTimeout(() => initPlayer(app, token, retries + 1), 1000);
    return;
  }

  player = new Spotify.Player({
    name: "Pelmodoro",
    getOAuthToken: (cb) => cb(token),
    volume: 1,
  });

  player.addListener("ready", ({ device_id }) => {
    window.spotify.canPlay = true;
    window.spotify.deviceId = device_id;
  });

  player.addListener("not_ready", ({ device_id }) => {
    window.spotify.canPlay = false;
    window.spotify.deviceId = device_id;
  });

  player.addListener("authentication_error", () => notConnected(app));
  player.addListener("account_error", () => connectionError(app));

  player.connect();
};

const initApp = (app: ElmApp, flash = true): void => {
  const authData = authDataDecoder(storage.get("spotifyAuthData", {}));

  if (authData.access_token) {
    const now = Date.now();

    if (now > authData.expires_at) {
      refreshAuthToken(authData.refresh_token)
        .catch(() => notConnected(app))
        .then((data) => {
          if (!data) return;

          storage.set("spotifyAuthData", data);

          init(app, data.access_token, flash);
        });
    } else {
      const expiresDiff = authData.expires_at - now;
      const timeout = Math.floor(expiresDiff) + 1000;

      setTimeout(() => initApp(app), timeout);

      window.spotify.connected = true;

      init(app, authData.access_token, flash);
    }
  } else {
    notConnected(app, flash);
  }
};

interface ReqParams {
  method: "PUT";
  headers: Record<string, string>;
}

const apiReqParams = (token: string): ReqParams => {
  return {
    method: "PUT",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  };
};

const pause = (token: string): void => {
  if (window.spotify.playing == true) {
    checkStateReq(token)
      .then(promiseByStatus)
      .then((state) => storage.set("spotifyLastState", state))
      .finally(() => {
        window.spotify.playing = false;
        fetch(
          `https://api.spotify.com/v1/me/player/pause?device_id=${window.spotify.deviceId}`,
          apiReqParams(token)
        );
      });
  }
};

const play = (token: string, uri: string): void => {
  if (window.spotify.canPlay == false) {
    return;
  }

  const deviceId = window.spotify.deviceId;
  const lastState = decodeWith(playbackState, storage.get("spotifyLastState"));

  let body: { context_uri: string; position_ms?: number | null } = {
    context_uri: uri,
  };

  if (lastState.status === "ok") {
    body = { ...body, position_ms: lastState.data.progress_ms };
  }

  fetch(`https://api.spotify.com/v1/me/player/play?device_id=${deviceId}`, {
    ...apiReqParams(token),
    body: JSON.stringify(body),
  }).then((res) => {
    if (![202, 204].includes(res.status)) {
      window.spotify.playing = false;
      return;
    }

    window.spotify.playing = true;
  });
};

const checkStateReq = (token: string): Promise<Response> => {
  return fetch(`https://api.spotify.com/v1/me/player`, {
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  });
};

const checkState = (token: string): void => {
  setInterval(() => {
    if (window.spotify.playing == true) {
      checkStateReq(token)
        .then(promiseByStatus)
        .then((state) => {
          storage.set("spotifyLastState", state);
        });
    }
  }, 30 * 1000);
};

const disconnect = (app: ElmApp): void => {
  storage.del("spotifyLastState");
  storage.del("spotifyAuthData");
  storage.del("spotifyConnectData");

  window.spotify.connected = false;
  window.spotify.canPlay = false;
  window.spotify.playing = false;
  window.spotify.deviceId = null;

  player?.disconnect();
  notConnected(app);
};

const init = (app: ElmApp, token: string, flash: boolean): void => {
  initPlayer(app, token, 0);
  setupPlaylists(app, token, flash);
  checkState(token);

  app.ports.toSpotify.subscribe((data: ToSpotifyPayload) => {
    switch (data.type) {
      case "play":
        play(token, data.url);
        break;

      case "pause":
        pause(token);
        break;

      case "refresh":
        setupPlaylists(app, token, true);
        break;

      case "disconnect":
        disconnect(app);
        break;
    }
  });
};

export default function(app: ElmApp): void {
  const query = new URLSearchParams(window.location.search);
  const code = query.get("code");
  const state = query.get("state");

  if (window.location.pathname == redirectUrl.pathname && code && state) {
    connectionCallback(app, code, state);
  } else {
    initApp(app, false);
  }
}
