import {
  array,
  DecoderFunction,
  decodeType,
  nullable,
  number,
  record,
  string,
} from "typescript-json-decoder";
import { Result } from "./result";

export function decodeWith<T>(decoder: DecoderFunction<T>, data: unknown): Result<T, string> {
  try {
    const decoded = decoder(data);

    return { status: "ok", data: decoded };
  } catch (e: unknown) {
    let error = "Unknown error";

    if (typeof e === "string") {
      error = e;
    }

    return { status: "err", error }
  }
}

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
