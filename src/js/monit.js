import * as Sentry from "@sentry/browser";
import { Integrations } from "@sentry/tracing";

Sentry.init({
  dsn: import.meta.env.VITE_SENTRY_DSN,
  integrations: [new Integrations.BrowserTracing()],
  tracesSampleRate: parseFloat(import.meta.env.VITE_SENTRY_SAMPLE_RATE)
});
