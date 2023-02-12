import pkceChallenge from "pkce-challenge";
import randomString from "crypto-random-string";

import * as storage from "./helpers/local-storage";
import setFlash from "./helpers/flash";

const clientId = import.meta.env.VITE_SPOTIFY_CLIENT_ID;
const redirectUri = import.meta.env.VITE_SPOTIFY_REDIRECT_URL;
const redirectUrl = new URL(redirectUri);

let player: unknown;

export type SpotifyDef = {
  connected: boolean;
  canPlay: boolean;
  playing: boolean;
  deviceId: string | null;
}

const spotify: SpotifyDef = {
  connected: false,
  canPlay: false,
  playing: false,
  deviceId: null,
}

window.spotify = spotify;

const connectData = () => {
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

const processAuthData = (authData) => {
  const now = Date.now();
  const expiresAt = now + authData.expires_in * 1000;

  authData = { ...authData, expires_at: expiresAt };

  storage.set("spotifyAuthData", authData);

  return authData;
};

const promiseByStatus = (res) => {
  if (res.status != 200) {
    return Promise.reject(null);
  } else {
    return res.json();
  }
};

const getPlaylists = (token) => {
  return fetch("https://api.spotify.com/v1/me/playlists?limit=50", {
    headers: { Authorization: `Bearer ${token}` },
  })
    .then(promiseByStatus)
    .then((data) => {
      return Promise.resolve(
        data.items.map((item) => {
          return { uri: item.uri, title: item.name };
        })
      );
    });
};

const authRequest = (body) => {
  return fetch("https://accounts.spotify.com/api/token", {
    method: "POST",
    body: body,
  })
    .then(promiseByStatus)
    .then((authData) => {
      window.spotify.connected = true;

      return Promise.resolve(processAuthData(authData));
    });
};

const refreshAuthToken = (token) => {
  const body = new URLSearchParams();

  body.append("client_id", clientId);
  body.append("grant_type", "refresh_token");
  body.append("refresh_token", token);

  return authRequest(body);
};

const getAuthToken = (code, state) => {
  const connectData = storage.get("spotifyConnectData", {});

  if (state != connectData.state) {
    return false;
  }

  const body = new URLSearchParams();

  body.append("client_id", clientId);
  body.append("grant_type", "authorization_code");
  body.append("code", code);
  body.append("redirect_uri", redirectUri);
  body.append("code_verifier", connectData.code_verifier);

  return authRequest(body);
};

const notConnected = (app, flash = true) => {
  app.ports.gotFromSpotify.send({
    type: "notconnected",
    url: connectData().url,
  });

  if (flash) {
    setFlash(app, "Your Spotify account is disconnected.");
  }
};

const connectionError = (app) => {
  app.ports.gotFromSpotify.send({
    type: "connectionerror",
    url: connectData().url,
  });
  setFlash(
    app,
    "There was an error trying to connect to your Spotify account. Note: you need a Premium account to connect."
  );
};

const connected = (app, playlists, flash = true) => {
  app.ports.gotFromSpotify.send({
    type: "connected",
    playlists: playlists,
    playlist: null,
  });

  if (flash) {
    setFlash(app, "Your Spotify account is connected.");
  }
};

const setupPlaylists = (app, token, flash) => {
  getPlaylists(token)
    .then((playlists) => connected(app, playlists, flash))
    .catch(() => notConnected(app, flash));
};

const connectionCallback = (app, code, state) => {
  getAuthToken(code, state)
    .then((data) => init(app, data.access_token))
    .catch(() => connectionError(app));

  history.pushState({}, "", redirectUrl.pathname);
};

const initPlayer = (app, token, retries) => {
  if (retries > 9) {
    return false;
  }

  if (
    window.spotifyPlayerLoaded == false ||
    window.spotify.connected == false
  ) {
    return setTimeout(() => initPlayer(app, token, retries + 1), 1000);
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

const initApp = (app, flash = true) => {
  const authData = storage.get("spotifyAuthData", {});

  if (authData.access_token) {
    const now = Date.now();

    if (now > authData.expires_at) {
      refreshAuthToken(authData.refresh_token)
        .catch(() => notConnected(app))
        .then((data) => {
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

const apiReqParams = (token) => {
  return {
    method: "PUT",
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  };
};

const pause = (token) => {
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

const play = (token, uri) => {
  if (window.spotify.canPlay == false) {
    return false;
  }

  const deviceId = window.spotify.deviceId;

  const lastState = storage.get("spotifyLastState", { context: { uri: null } });
  let body = { context_uri: uri };

  if (lastState.context.uri == uri) {
    body = { ...body, position_ms: lastState.progress_ms };
  }

  fetch(`https://api.spotify.com/v1/me/player/play?device_id=${deviceId}`, {
    ...apiReqParams(token),
    body: JSON.stringify(body),
  }).then((res) => {
    if (res.status != 204) {
      window.spotify.playing = false;

      return false;
    }

    window.spotify.playing = true;
  });
};

const checkStateReq = (token) => {
  return fetch(`https://api.spotify.com/v1/me/player`, {
    headers: {
      "Content-Type": "application/json",
      Authorization: `Bearer ${token}`,
    },
  });
};

const checkState = (token) => {
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

const disconnect = (app) => {
  storage.del("spotifyLastState");
  storage.del("spotifyAuthData");
  storage.del("spotifyConnectData");

  window.spotify.connected = false;
  window.spotify.canPlay = false;
  window.spotify.playing = false;
  window.spotify.deviceId = null;

  player.disconnect();
  notConnected(app);
};

const init = (app, token, flash) => {
  initPlayer(app, token, 0);
  setupPlaylists(app, token, flash);
  checkState(token);

  app.ports.toSpotify.subscribe((data) => {
    switch (data["type"]) {
      case "play":
        play(token, data["url"]);
        break;

      case "pause":
        pause(token);
        break;

      case "refresh":
        setupPlaylists(app, token);
        break;

      case "disconnect":
        disconnect(app);
        break;
    }
  });
};

export default function (app) {
  const query = new URLSearchParams(window.location.search);
  const code = query.get("code");
  const state = query.get("state");

  if (window.location.pathname == redirectUrl.pathname && code && state) {
    connectionCallback(app, code, state);
  } else {
    initApp(app, false);
  }
}
