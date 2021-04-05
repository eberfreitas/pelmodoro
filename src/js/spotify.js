import pkceChallenge from "pkce-challenge";
import randomString from "crypto-random-string";

import * as storage from "./helpers/local-storage.js";

const clientId = process.env.SPOTIFY_CLIENT_ID;
const redirectUri = process.env.SPOTIFY_REDIRECT_URL;

const connectData = () => {
  const pkce = pkceChallenge(128);
  const state = randomString({ length: 16, type: "url-safe" });

  const url = `https://accounts.spotify.com/authorize?client_id=${clientId}`
    + `&response_type=code`
    + `&redirect_uri=${encodeURI(redirectUri)}`
    + `&state=${state}`
    + `&code_challenge_method=S256`
    + `&code_challenge=${pkce.code_challenge}`
    + `&scope=playlist-read-private,playlist-read-collaborative`

  const data = { ...pkce, state, url };

  storage.set("spotifyConnectData", data);

  return data;
};

const processTokenData = tokenData => {
  const now = Date.now();
  const expiresAt = now + (tokenData.expires_in * 1000);

  tokenData = { ...tokenData, expires_at: expiresAt };

  storage.set("spotifyTokenData", tokenData);

  return tokenData;
};

const promiseByStatus = res => {
  if (res.status != 200) {
    return Promise.reject(null);
  } else {
    return res.json();
  }
};

const tokenRequest = body => {
  return fetch("https://accounts.spotify.com/api/token", {
    method: "POST",
    body: body
  })
    .then(promiseByStatus)
    .then(tokenData => Promise.resolve(processTokenData(tokenData)));
};

const refreshToken = token => {
  let body = new URLSearchParams();

  body.append("client_id", clientId);
  body.append("grant_type", "refresh_token");
  body.append("refresh_token", token);

  return tokenRequest(body);
};

const getToken = (code, state) => {
  const connectData = storage.get("spotifyConnectData", {});

  if (state != connectData.state) {
    return false;
  }

  let body = new URLSearchParams();

  body.append("client_id", clientId);
  body.append("grant_type", "authorization_code");
  body.append("code", code);
  body.append("redirect_uri", redirectUri);
  body.append("code_verifier", connectData.code_verifier);

  return tokenRequest(body);
};

const getPlaylists = token => {
  return fetch("https://api.spotify.com/v1/me/playlists?limit=50", {
    headers: { "Authorization": `Bearer ${token}` }
  })
    .then(promiseByStatus)
    .then(data => {
      return Promise.resolve(data.items.map(item => {
        return { uri: item.uri, title: item.name }
      }));
    });
};

const notConnected = app => {
  app.ports.gotSpotifyState.send({ type: "notconnected", url: connectData().url });
};

const connectionError = app => {
  app.ports.gotSpotifyState.send({ type: "connectionerror", url: connectData().url });
};

const connected = (app, playlists) => {
  app.ports.gotSpotifyState.send({ type: "connected", playlists: playlists, playlist: null });
};

const setupPlaylists = (app, token) => {
  getPlaylists(token)
    .catch(() => notConnected(app))
    .then(playlists => connected(app, playlists));
};

const connectionCallback = app => {
  const query = new URLSearchParams(window.location.search);
  const code = query.get("code") || "";
  const state = query.get("state") || "";

  if (code && state) {
    getToken(code, state)
      .then(data => setupPlaylists(app, data.access_token))
      .catch(() => connectionError(app));
  }
};

const init = app => {
  const tokenData = storage.get("spotifyTokenData", {});

  if (tokenData.access_token) {
    const now = Date.now();

    if (now > tokenData.expires_at) {
      refreshToken(tokenData.refresh_token)
        .catch(() => notConnected(app))
        .then(data => {
          storage.set("spotifyTokenData", data);
          setupPlaylists(app, data.access_token);
        });
    } else {
      setupPlaylists(app, tokenData.access_token);
    }
  } else {
    notConnected(app);
  }
};

export default function (app) {
  if (window.location.pathname == "/callback") {
    connectionCallback(app);
  } else {
    init(app);
  }
}
