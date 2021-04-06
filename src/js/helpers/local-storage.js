export const get = (key, defVal) => {
  let parsed;

  try {
    parsed = JSON.parse(window.localStorage.getItem(key));
  } catch (_) {
    parsed = defVal;
  }

  if (parsed == null) {
    parsed = defVal;
  }

  return parsed;
};

export const set = (key, data) => {
  return window.localStorage.setItem(key, JSON.stringify(data));
};

export const del = key => {
  return window.localStorage.removeItem(key);
};
