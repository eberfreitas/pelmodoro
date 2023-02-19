import {
  array,
  decodeType,
  nullable,
  number,
  record,
  string,
} from "typescript-json-decoder";

export const spotifyPlaylist = record({
  items: array(
    record({
      uri: string,
      name: string,
    })
  ),
});

export const spotifyApiToken = record({
  access_token: string,
  token_type: string,
  scope: string,
  expires_in: number,
  refresh_token: string,
});

export type SpotifyApiToken = decodeType<typeof spotifyApiToken>;

export const authData = record({
  access_token: string,
  token_type: string,
  scope: string,
  expires_in: number,
  expires_at: number,
  refresh_token: string,
});

export type AuthData = decodeType<typeof authData>;

export const spotifyConnectData = record({
  url: string,
  state: string,
  code_verifier: string,
  code_challenge: string,
});

export type SpotifyConnectData = decodeType<typeof spotifyConnectData>;

export const playbackState = record({
  progress_ms: nullable(number),
  context: record({
    uri: string,
  }),
});

export type PlaybackState = decodeType<typeof playbackState>;
