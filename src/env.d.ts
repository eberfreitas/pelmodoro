interface ImportMetaEnv {
  readonly VITE_SPOTIFY_CLIENT_ID: string;
  readonly VITE_SPOTIFY_REDIRECT_URL: string;
  readonly VITE_SENTRY_DSN: string;
  readonly VITE_SENTRY_SAMPLE_RATE: string;
  readonly VITE_ENVIRONMENT: string;
}

interface ImportMeta extends ImportMeta {
  readonly env: ImportMetaEnv;
}
