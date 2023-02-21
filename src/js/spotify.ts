import randomString from "crypto-random-string";
import pkceChallenge from "pkce-challenge";
import { DecoderFunction } from "typescript-json-decoder";
import { ElmApp, SpotifyDef, ToSpotifyPayload } from "../globals";
import {
  AuthData,
  authData as authDataDecoder,
  decodeWith,
  PlaybackState,
  playbackState,
  spotifyApiToken,
  SpotifyApiToken,
  SpotifyConnectData,
  spotifyConnectData,
  spotifyPlaylist,
} from "./decoders";
import setFlash from "./helpers/flash";
import * as storage from "./helpers/local-storage";
import { Result, resultCallback, resultErr, resultMap } from "./result";

const clientId = import.meta.env.VITE_SPOTIFY_CLIENT_ID;
const redirectUri = import.meta.env.VITE_SPOTIFY_REDIRECT_URL;
const redirectUrl = new URL(redirectUri);

let player: Spotify.Player | undefined;

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

async function fetchAsJsonResult<T>(
  url: string,
  init: RequestInit,
  decoder: DecoderFunction<T>
): Promise<Result<T>> {
  try {
    const response = await fetch(url, init);
    const json: unknown = await response.json();

    return decodeWith(decoder, json);
  } catch (e) {
    return resultErr(
      new Error(
        `Could not process request to: ${url} | ERROR: ${JSON.stringify(e)}`
      )
    );
  }
}

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

const getPlaylists = async (token: string): Promise<Result<Playlist[]>> => {
  const result = await fetchAsJsonResult(
    "https://api.spotify.com/v1/me/playlists?limit=50",
    { headers: { Authorization: `Bearer ${token}` } },
    spotifyPlaylist
  );

  return resultMap(result, (data) =>
    data.items.map((item) => ({
      uri: item.uri,
      title: item.name,
    }))
  );
};

const authRequest = async (
  body: URLSearchParams
): Promise<Result<AuthData>> => {
  const result = await fetchAsJsonResult(
    "https://accounts.spotify.com/api/token",
    { method: "POST", body },
    spotifyApiToken
  );

  return resultMap(result, (data) => {
    window.spotify.connected = true;

    return processAuthData(data);
  });
};

const refreshAuthToken = async (token: string): Promise<Result<AuthData>> => {
  const body = new URLSearchParams();

  body.append("client_id", clientId);
  body.append("grant_type", "refresh_token");
  body.append("refresh_token", token);

  return authRequest(body);
};

const getAuthToken = (
  code: string,
  state: string
): Promise<Result<AuthData>> => {
  const result = decodeWith(
    spotifyConnectData,
    storage.get("spotifyConnectData")
  );

  if (result.status === "err") {
    return Promise.resolve(result);
  }

  const connectData = result.data;

  if (state != connectData.state) {
    return Promise.resolve(resultErr(new Error("Invalid `state` value.")));
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
  void getPlaylists(token).then((result) => {
    resultCallback(
      result,
      (playlists) => connected(app, playlists, flash),
      () => notConnected(app, flash)
    );
  });
};

const connectionCallback = (app: ElmApp, code: string, state: string): void => {
  void getAuthToken(code, state).then((result) => {
    resultCallback(
      result,
      (data) => init(app, data.access_token, true),
      () => connectionError(app)
    );
  });

  history.pushState({}, "", redirectUrl.pathname);
};

const initPlayer = (app: ElmApp, token: string, retries: number): void => {
  if (retries > 9) {
    return;
  }

  if (!window.spotifyPlayerLoaded || !window.spotify.connected) {
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

  void player.connect();
};

const initApp = (app: ElmApp, flash = true): void => {
  const result = decodeWith(authDataDecoder, storage.get("spotifyAuthData"));

  const okCallback = async (authData: AuthData) => {
    const now = Date.now();

    if (now > authData.expires_at) {
      const refreshResult = await refreshAuthToken(authData.refresh_token);

      resultCallback(
        refreshResult,
        (data) => {
          storage.set("spotifyAuthData", data);
          init(app, data.access_token, flash);
        },
        () => notConnected(app)
      );
    } else {
      const expiresDiff = authData.expires_at - now;
      const timeout = Math.floor(expiresDiff) + 1000;

      setTimeout(() => initApp(app), timeout);

      window.spotify.connected = true;

      init(app, authData.access_token, flash);
    }
  };

  const errCallback = () => notConnected(app, flash);

  resultCallback(result, okCallback, errCallback);
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
  if (!window.spotify.playing) return;

  void checkStateReq(token).then((result) => {
    resultCallback(result, (state) => storage.set("spotifyLastState", state));
  });

  const deviceId = window.spotify.deviceId ?? "";

  void fetch(
    `https://api.spotify.com/v1/me/player/pause?device_id=${deviceId}`
  ).then(() => (window.spotify.playing = false));
};

const play = (token: string, uri: string): void => {
  if (!window.spotify.canPlay) return;

  const deviceId = window.spotify.deviceId ?? "";
  const lastState = decodeWith(playbackState, storage.get("spotifyLastState"));

  let body: { context_uri: string; position_ms?: number | null } = {
    context_uri: uri,
  };

  if (lastState.status === "ok") {
    body = { ...body, position_ms: lastState.data.progress_ms };
  }

  void fetch(
    `https://api.spotify.com/v1/me/player/play?device_id=${deviceId}`,
    {
      ...apiReqParams(token),
      body: JSON.stringify(body),
    }
  ).then((res) => {
    if (![202, 204].includes(res.status)) {
      window.spotify.playing = false;
    } else {
      window.spotify.playing = true;
    }
  });
};

const checkStateReq = (token: string): Promise<Result<PlaybackState>> => {
  return fetchAsJsonResult(
    "https://api.spotify.com/v1/me/player",
    {
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${token}`,
      },
    },
    playbackState
  );
};

const checkState = (token: string): void => {
  setInterval(() => {
    if (!window.spotify.playing) return;

    void checkStateReq(token).then((result) =>
      resultCallback(result, (state) => storage.set("spotifyLastState", state))
    );
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
