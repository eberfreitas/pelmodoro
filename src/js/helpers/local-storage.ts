export const get = (key: string, defVal: unknown = null): unknown => {
  let parsed: unknown;

  try {
    parsed = JSON.parse(window.localStorage.getItem(key) || "");
  } catch (_) {
    parsed = defVal;
  }

  if (parsed == null) {
    parsed = defVal;
  }

  return parsed;
};

export const set = (key: string, data: unknown): void => {
  window.localStorage.setItem(key, JSON.stringify(data));
};

export const del = (key: string): void => {
  window.localStorage.removeItem(key);
};
